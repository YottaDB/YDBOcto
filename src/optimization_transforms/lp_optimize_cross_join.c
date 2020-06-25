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
#include "logical_plan.h"

/* Currently the only join types that are compatible with CROSS_JOIN reordering is NATURAL_JOIN.
 * INNER and OUTER JOINs have issues (see TC09.sql and TC10.sql) that may be addressed at a later point in time.
 */
#define IS_CROSS_JOIN_COMPATIBLE(JOIN_TYPE) ((CROSS_JOIN == JOIN_TYPE) || (NATURAL_JOIN == JOIN_TYPE))

/* This function reorders CROSS JOINs in the FROM/JOIN list so as to maximize the number of key fixing optimizations (#529).
 *
 * "table_join" is the start of the list of tables/joins in this query.
 * "where" is a pointer to the WHERE clause.
 */
void lp_optimize_cross_join(LogicalPlan *plan, LogicalPlan *table_join, LogicalPlan *where) {
	int	      num_cross_joins, max_unique_id, cur_entries, max_entries, i, j, tmp_id;
	LogicalPlan **table_join_array; /* a 1-d array of pointers to LP_TABLE_JOIN "LogicalPlan" */
	boolean_t *   equals_id1_id2;	/* a 2-d array indicating whether the WHERE clause has a condition
					 * of the form TBL1.COL1 = TBL2.COL2 where the indexes into this array
					 * (id1 and id2) are the "unique_id" values of TBL1 and TBL2.
					 */
	int *sorted_array;		/* a 1-d array storing the unique_id of the table in the sorted order of
					 * CROSS JOINs.
					 */
	boolean_t *id_seen_array;	/* a 1-d array storing whether an id has already been added to
					 * "sorted_array".
					 */
	LogicalPlan *	 left, *right, *start_right, *next_left, *next_right, *cur_table_join, *tmp;
	enum SqlJoinType next_join_type;

	assert(LP_TABLE_JOIN == table_join->type);
	/* Allocate an array of table joins (instead of the linked list that we come in with) as it helps do things faster.
	 * Towards that, first find number of CROSS JOINs that can potentially be moved to the front of the FROM/JOIN list.
	 * Note that CROSS JOINs can be safely reordered without correctness issues as long as there is no inner or outer join
	 * in the query (see outer join example query at `TC09.sql` and inner join example query at `TC10.sql`). Moving them
	 * to the front helps us later do reordering within them for query optimization. Also note that the order of
	 * non-CROSS-JOINs in the table join list has to be preserved as is (or else we would have correctness issues).
	 */
	left = table_join;
	/* Find first tablejoin that is followed by a non-CROSS-JOIN. After this is the point where a CROSS JOIN
	 * (if any, found later in the tablejoin linked list) will be moved into.
	 */
	num_cross_joins = 0;
	assert(NULL != left);
	do {
		next_left = left->v.lp_default.operand[1];
		if (NULL == next_left) {
			break;
		}
		next_join_type = next_left->extra_detail.lp_table_join.cur_join_type;
		if (!IS_CROSS_JOIN_COMPATIBLE(next_join_type)) {
			return; /* Found at least one incompatible JOIN type. No optimization possible currently */
		}
		if (CROSS_JOIN != next_join_type) {
			break;
		}
		num_cross_joins++;
		left = next_left;
	} while (NULL != left);
	right = start_right = next_left;
	if (NULL != right) {
		for (;;) {
			/* Find first tablejoin after "right" that precedes a CROSS JOIN. This CROSS JOIN will be moved from here
			 * and inserted after "left" noted down above.
			 */
			for (; NULL != right; right = next_right) {
				next_right = right->v.lp_default.operand[1];
				if (NULL == next_right) {
					break;
				}
				next_join_type = next_right->extra_detail.lp_table_join.cur_join_type;
				if (!IS_CROSS_JOIN_COMPATIBLE(next_join_type)) {
					return; /* Found at least one incompatible JOIN type. No optimization possible currently */
				}
				if (CROSS_JOIN == next_join_type) {
					break;
				}
			}
			if (NULL == next_right) {
				break; /* No more CROSS JOIN moves possible */
			}
			/* Move "next_right" in between "left" and "next_left" */
			left->v.lp_default.operand[1] = next_right;
			right->v.lp_default.operand[1] = next_right->v.lp_default.operand[1];
			next_right->v.lp_default.operand[1] = next_left;
			left = next_right;
			num_cross_joins++;
		}
	}
	if (0 == num_cross_joins) {
		return; /* We need at least 1 CROSS JOIN for reordering to be possible */
	}
	/* Now that all CROSS JOINs possible are placed at the beginning of the tablejoin linked list (it is still possible that
	 * some CROSS JOINs after OUTER JOINs are not placed at the beginning of the tablejoin linked list), next determine the
	 * optimal join order by looking at the ON and/or WHERE clauses. Pick that join order which maximizes the number of key
	 * fixing optimizations that can happen.
	 */
	/* Maintain a list of the unique_ids corresponding to the tables in the CROSS JOIN list */
	max_unique_id = config->plan_id; /* similar logic exists in "lp_optimize_where_multi_equals_ands()" */
	table_join_array = (LogicalPlan **)calloc(max_unique_id, sizeof(LogicalPlan *)); /* "calloc" so it is NULL initialized */
	assert((NULL == start_right) || (CROSS_JOIN != start_right->extra_detail.lp_table_join.cur_join_type));
	for (cur_table_join = table_join; start_right != cur_table_join; cur_table_join = cur_table_join->v.lp_default.operand[1]) {
		int unique_id;

		assert(((cur_table_join == table_join) && (NO_JOIN == cur_table_join->extra_detail.lp_table_join.cur_join_type))
		       || ((cur_table_join != table_join)
			   && (CROSS_JOIN == cur_table_join->extra_detail.lp_table_join.cur_join_type)));
		unique_id = lp_get_tablejoin_unique_id(cur_table_join);
		assert(unique_id < max_unique_id);
		assert(0 < unique_id);
		assert(NULL == table_join_array[unique_id]);
		table_join_array[unique_id] = cur_table_join;
	}
	equals_id1_id2 = (boolean_t *)calloc(max_unique_id * max_unique_id, sizeof(boolean_t));
	/* The below function invocation fills in "equals_id1_id2" array that is needed by the CROSS JOIN reordering optimization */
	lp_optimize_where_multi_equals_ands_helper(plan, where, NULL, equals_id1_id2, max_unique_id);
	max_entries = num_cross_joins + 2; /* + 2 needed to add unique_id of first table in FROM list (which is not counted
					    * in "num_cross_joins" AND for 0 unique_id to kickstart the algorithm.
					    */
	sorted_array = (int *)malloc(max_entries * sizeof(int)); /* malloc enough (no need of calloc) */
	id_seen_array = (boolean_t *)calloc(max_unique_id, sizeof(boolean_t));
	/* The algorithm proceeds by finding WHERE clause operations that fix a column to a constant (e.g. "n1.firstname = 'Zero'").
	 * All tables involved in such expressions ("n1" in this example) get reordered to the beginning of the CROSS JOIN list.
	 * In the next iteration, we find all expressions in the WHERE clause that fix a column to a column from an already
	 * fixed table (e.g. "n2.lastname = n1.lastname"). All such tables are reordered next (e.g. "n2" gets added after "n1"
	 * in the CROSS JOIN reordered list in this example). And so on until we are done processing all CROSS JOINs.
	 */
	cur_entries = 0;
	sorted_array[cur_entries++] = 0; /* this will kickstart the algorithm */
	for (i = 0; i < cur_entries; i++) {
		tmp_id = sorted_array[i] * max_unique_id;
		for (j = 0; j < max_unique_id; j++) {
			if (id_seen_array[j]) {
				continue;
			}
			if (NULL == table_join_array[j]) {
				continue;
			}
			if (equals_id1_id2[tmp_id + j]) {
				assert(cur_entries < max_entries);
				sorted_array[cur_entries++] = j;
				id_seen_array[j] = TRUE;
			}
		}
	}
	/* At this point "sorted_array[]" contains the unique_ids of the tables in the sorted order.
	 * Fix the LP_TABLE_JOIN linked list to be in that order.
	 */
	cur_table_join = table_join;
	for (i = 1; i < cur_entries; i++) {
		tmp_id = sorted_array[i];
		right = table_join_array[tmp_id];
		if (cur_table_join != right) {
			/* Swap "right" and "cur_table_join" */
			tmp_id = lp_get_tablejoin_unique_id(cur_table_join);
			assert(cur_table_join == table_join_array[tmp_id]);
			table_join_array[tmp_id] = right;
			tmp = right->v.lp_default.operand[0];
			right->v.lp_default.operand[0] = cur_table_join->v.lp_default.operand[0];
			cur_table_join->v.lp_default.operand[0] = tmp;
		}
		/* else: This table join is already in the right position. Move on. */
		assert(cur_table_join != start_right);
		cur_table_join = cur_table_join->v.lp_default.operand[1];
	}
	free(table_join_array);
	free(equals_id1_id2);
	free(sorted_array);
	free(id_seen_array);
	return;
}
