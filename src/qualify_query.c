/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

int qualify_query(SqlStatement *stmt, SqlJoin *parent_join) {
	int result = 0;
	assert(stmt->type == table_alias_STATEMENT || stmt->type == set_operation_STATEMENT);
	if(stmt->type == table_alias_STATEMENT) {
		SqlTableAlias *table_alias;
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		if(table_alias->table->type == table_STATEMENT) {
			return result;
		}
		SqlSelectStatement *select;
		UNPACK_SQL_STATEMENT(select, table_alias->table, select);
		SqlJoin *join;
		UNPACK_SQL_STATEMENT(join, select->table_list, join);

		// Make note of my join so children can use them
		SqlJoin *prev_start, *prev_end, *temp_join;
		prev_start = join;
		prev_end = join->prev;
		if(parent_join != NULL) {
			dqinsert(join, parent_join, temp_join);
		}
		// First qualify subqueries
		SqlJoin *start_join, *cur_join;
		start_join = cur_join = join;
		/* Ensure strict column name qualification checks (i.e. all column name references have to be a valid column
		 * name in a valid existing table) by using MATCH_QUALIFIED_COLUMNS_TRUE below.
		 */
		do {
			result |= qualify_query(cur_join->value, NULL);
			result |= qualify_statement(cur_join->condition, join, stmt, MATCH_QUALIFIED_COLUMNS_TRUE);
			cur_join = cur_join->next;
		} while(cur_join != start_join && cur_join != parent_join);
		result |= qualify_statement(select->select_list, join, stmt, MATCH_QUALIFIED_COLUMNS_TRUE);
		result |= qualify_statement(select->where_expression, join, stmt, MATCH_QUALIFIED_COLUMNS_TRUE);
		/* Now that all column names used in the query have been validated, allow columns specified in
		 * ORDER BY to be validated against any column names specified till now without any strict checking.
		 * Hence the MATCH_QUALIFIED_COLUMNS_FALSE use below.
		 */
		result |= qualify_statement(select->order_expression, join, stmt, MATCH_QUALIFIED_COLUMNS_FALSE);

		// Make sure to reset parent query
		if(parent_join != NULL) {
			parent_join->prev = join->prev;
			parent_join->prev->next = parent_join;
			join->prev = prev_end;
			join->prev->next = prev_start;
		}
	} else if(stmt->type == set_operation_STATEMENT) {
		SqlSetOperation *set_opr;
		UNPACK_SQL_STATEMENT(set_opr, stmt, set_operation);
		result |= qualify_query(set_opr->operand[0], parent_join);
		result |= qualify_query(set_opr->operand[1], parent_join);
	} else {
		result = 1;
	}
	return result;
}
