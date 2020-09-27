/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"

/* This is an optimization function that tries to determine if it is safe to move a portion of the WHERE clause
 * (whose address is passed in as "stmt_ptr") to the ON clause of some JOIN in the query. If so, it moves it.
 * This can significantly reduce query runtime in case the query has lots of joins (the ON clause in one join
 * noticeably reduces the number of iterations a nested join executes compared to the same condition in the
 * WHERE clause which gets executed in the innermost loop for all nested join FOR loops) (YDBOcto#601).
 */
void move_where_clause_to_on_clause(SqlStatement **stmt_ptr, SqlJoin *start_join) {
	int	      max_unique_id;
	SqlJoin *     cur_join, *noted_join;
	boolean_t     outer_join_seen, right_or_full_join_seen;
	SqlStatement *stmt;

	stmt = *stmt_ptr;
	max_unique_id = stmt->hash_canonical_query_cycle;
	cur_join = start_join;
	outer_join_seen = FALSE;
	right_or_full_join_seen = FALSE;
	noted_join = NULL; /* This notes down the JOIN whose ON clause/condition will inherit the WHERE clause portion */
	do {
		enum SqlJoinType join_type;
		SqlJoin *	 next_join;

		next_join = cur_join->next;
		join_type = cur_join->type;
		if (IS_OUTER_JOIN(join_type)) {
			outer_join_seen = TRUE;
			if ((RIGHT_JOIN == join_type) || (FULL_JOIN == join_type)) {
				right_or_full_join_seen = TRUE;
			}
		} else if ((NULL == noted_join) && (CROSS_JOIN != join_type) && (CROSS_JOIN != next_join->type)
			   && (next_join != start_join) && (max_unique_id <= cur_join->max_unique_id)) {
			/* CROSS JOIN does not currently expect an ON clause ("lp_optimize_cross_join()" relies on this).
			 * Hence the "CROSS_JOIN != join_type" and "CROSS_JOIN != next_join->type" check above. Also, if the
			 * join we are going to move the WHERE clause to is the last in the join list, we might as well keep
			 * it inside the WHERE clause (no savings in runtime) hence the check for "next_join != start_join" above.
			 * The "max_unique_id <= cur_join->max_unique_id" check is the key check. It confirms that it is safe
			 * to move the WHERE clause to the ON clause because the part of the WHERE clause we are considering
			 * moving has references to columns all of which correspond to tables that are known at the time the
			 * JOIN is parsed in the query.
			 */
			noted_join
			    = cur_join; /* Note: We still need to process rest of JOIN list to see if there is any OUTER JOIN */
		}
		cur_join = next_join;
	} while (cur_join != start_join);
	/* If we saw a RIGHT or FULL JOIN, the move from a WHERE clause to the ON clause cannot be safely done in all
	 * cases (TOJ03 bats subtest generates random queries which will fail otherwise). But moving in the presence of
	 * LEFT JOINs (the most frequently used outer join) are okay hence the check for "!right_or_full_join_seen" below.
	 * If RIGHT/FULL JOIN cases become more common and need to be optimized, this code needs to be revisited.
	 */
	if ((NULL != noted_join) && !right_or_full_join_seen) {
		/* Move operand of BOOLEAN_AND from WHERE clause to ON clause of this JOIN */
		if (NULL != noted_join->condition) {
			SqlStatement *join_condition;

			SQL_STATEMENT(join_condition, binary_STATEMENT);
			MALLOC_STATEMENT(join_condition, binary, SqlBinaryOperation);
			join_condition->v.binary->operation = BOOLEAN_AND;
			join_condition->v.binary->operands[0] = noted_join->condition;
			join_condition->v.binary->operands[1] = stmt;
			noted_join->condition = join_condition;
		} else {
			noted_join->condition = stmt;
		}
		/* If OUTER JOIN has been seen, then we cannot safely remove this from the WHERE clause
		 * (might return incorrect results). Adding the same condition to the ON clause does help
		 * speed up so we will do that but will also retain it in the WHERE clause.
		 */
		if (!outer_join_seen) {
			*stmt_ptr = NULL;
		}
	}
	return;
}
