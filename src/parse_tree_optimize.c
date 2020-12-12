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

void parse_tree_optimize(SqlSelectStatement *select) {
	SqlJoin *     cur_join, *next_join, *start_join;
	SqlStatement *stmt;

	/* Move bits and pieces of WHERE clause to ON clause if possible thereby speeding up query execution */
	stmt = select->where_expression;
	if (NULL != stmt) {
		SqlJoin *start_join;

		UNPACK_SQL_STATEMENT(start_join, select->table_list, join);
		if (IS_STMT_BOOLEAN_AND(stmt)) {
			select->where_expression = traverse_where_clause(stmt, start_join);
		} else {
			move_where_clause_to_on_clause(&select->where_expression, start_join);
		}
	}
	/* Check if any VALUES clause specifications in a subquery can be optimized. See comment in "octo_types.h"
	 * before "alternate_value" member in the "SqlJoin" structure for details.
	 */
	UNPACK_SQL_STATEMENT(start_join, select->table_list, join);
	next_join = NULL;
	for (cur_join = start_join; start_join != next_join; cur_join = next_join) {
		SqlStatement *tmp_value;

		next_join = cur_join->next;
		if (NULL == cur_join->alternate_value) {
			continue;
		}
		/* This is a case where we can optimize the outer query. The join can be replaced with the inner query join */
		tmp_value = cur_join->alternate_value;
		cur_join->alternate_value = cur_join->value;
		cur_join->value = tmp_value;
	}
	return;
}
