/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_types.h"

SqlColumnListAlias *columns_to_column_list_alias(SqlColumn *column, SqlStatement *table_alias_stmt) {
	SqlColumn *	    cur_column, *start_column;
	SqlColumnListAlias *ret;

	if (NULL == column)
		return NULL;
	cur_column = start_column = column;
	ret = NULL;
	do {
		if (!cur_column->is_hidden_keycol) {
			SqlColumnList *	    cur;
			SqlColumnAlias *    column_alias;
			SqlColumnListAlias *cur_column_list_alias;
			SqlStatement *	    stmt;

			SQL_STATEMENT(stmt, column_alias_STATEMENT);
			MALLOC_STATEMENT(stmt, column_alias, SqlColumnAlias);
			column_alias = stmt->v.column_alias;
			PACK_SQL_STATEMENT(column_alias->column, cur_column, column);
			assert(table_alias_STATEMENT == table_alias_stmt->type);
			column_alias->table_alias_stmt = table_alias_stmt;

			OCTO_CMALLOC_STRUCT(cur_column_list_alias, SqlColumnListAlias);
			cur_column_list_alias->alias = cur_column->columnName;
			/* Note: Unlike other callers of "get_sqlvaluetype_from_sqldatatype()", in this caller case, it is possible
			 * to see "UNKNOWN_SqlDataType" (in case of a VALUES() clause as we don't yet know the data type of the
			 * elements specified). The type will be later determined in "populate_data_type.c" so allow for an
			 * unknown type by passing "TRUE" as the second parameter below.
			 */
			cur_column_list_alias->type
			    = get_sqlvaluetype_from_sqldatatype(cur_column->data_type_struct.data_type, TRUE);
			OCTO_CMALLOC_STRUCT(cur, SqlColumnList);
			dqinit(cur);
			cur->value = stmt;
			PACK_SQL_STATEMENT(cur_column_list_alias->column_list, cur, column_list);
			dqinit(cur_column_list_alias);
			if (NULL == ret) {
				ret = cur_column_list_alias;
			} else {
				dqappend(ret, cur_column_list_alias);
			}
		} else {
			/* Do not consider hidden key columns as part of user-visible column list of a table */
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	assert(NULL != ret);
	return ret;
}
