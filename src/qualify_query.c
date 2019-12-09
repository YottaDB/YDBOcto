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
	int			result = 0;
	SqlColumnListAlias	*ret_cla;

	assert(stmt->type == table_alias_STATEMENT || stmt->type == set_operation_STATEMENT);
	if (stmt->type == table_alias_STATEMENT) {
		SqlTableAlias		*table_alias;
		SqlSelectStatement	*select;
		SqlJoin			*join;
		SqlJoin			*prev_start, *prev_end;
		SqlJoin			*start_join, *cur_join;

		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		if (table_STATEMENT == table_alias->table->type) {
			return result;
		}
		UNPACK_SQL_STATEMENT(select, table_alias->table, select);
		UNPACK_SQL_STATEMENT(join, select->table_list, join);

		// Make note of my join so children can use them
		prev_start = join;
		prev_end = join->prev;
		if (NULL != parent_join) {
			dqappend(join, parent_join);
		}
		// First qualify subqueries
		start_join = cur_join = join;
		/* Ensure strict column name qualification checks (i.e. all column name references have to be a valid column
		 * name in a valid existing table) by using NULL value (instead of "&ret_cla") in "qualify_statement" calls below.
		 */
		do {
			SqlJoin	*save_join;

			result |= qualify_query(cur_join->value, NULL);
			/* Make sure any table.column references in the ON condition of the JOIN (cur_join->condition)
			 * are validated until the current table in the join list (i.e. forward references should not be
			 * allowed). See YDBOcto#291 for example query. Note that NATURAL JOIN is an exception in that
			 * the current join has table.column references to the immediately next SqlJoin in the list.
			 * Since there is no ON clause in a NATURAL JOIN, there is no need to validate user-specified
			 * column names like one needs to in say a LEFT JOIN. Therefore skip this forward-reference-check
			 * related for a NATURAL JOIN.
			 */
			if (NATURAL_JOIN != cur_join->type) {
				save_join = cur_join->next;	/* save join list before tampering with it */
				/* Note that if "parent_join" is non-NULL, we need to include that even though it comes
				 * after all the tables in the join list at the current level. This is so any references
				 * to columns in parent queries are still considered as valid. Hence the parent_join check below.
				 */
				cur_join->next = ((NULL != parent_join) ? parent_join : start_join);	/* stop join list at
													 * current join.
													 */
			}
			result |= qualify_statement(cur_join->condition, start_join, stmt, 0, NULL);
			if (NATURAL_JOIN != cur_join->type)
				cur_join->next = save_join;	/* restore join list to original */
			cur_join = cur_join->next;
		} while(cur_join != start_join && cur_join != parent_join);
		result |= qualify_statement(select->select_list, start_join, stmt, 0, NULL);
		result |= qualify_statement(select->where_expression, start_join, stmt, 0, NULL);
		/* Now that all column names used in the query have been validated, allow columns specified in
		 * ORDER BY to be validated against any column names specified till now without any strict checking.
		 * Hence the non-NULL value of "&ret_cla" passed below to "qualify_statement".
		 */
		ret_cla = NULL;
		result |= qualify_statement(select->order_expression, start_join, stmt, 0, &ret_cla);
		// Make sure to reset parent query
		if (NULL != parent_join) {
			parent_join->prev = start_join->prev;
			parent_join->prev->next = parent_join;
			start_join->prev = prev_end;
			start_join->prev->next = prev_start;
		}
	} else if (set_operation_STATEMENT == stmt->type) {
		SqlSetOperation		*set_opr;

		UNPACK_SQL_STATEMENT(set_opr, stmt, set_operation);
		result |= qualify_query(set_opr->operand[0], parent_join);
		result |= qualify_query(set_opr->operand[1], parent_join);
	} else {
		result = 1;
	}
	return result;
}
