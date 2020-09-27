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
	return;
}
