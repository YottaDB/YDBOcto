/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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
		switch (cur_column->data_type_struct.data_type) {
		case UNKNOWN_SqlDataType:
			cur_column_list_alias->type = UNKNOWN_SqlValueType;
			break;
		case BOOLEAN_TYPE:
			cur_column_list_alias->type = BOOLEAN_VALUE;
			break;
		case INTEGER_TYPE:
			cur_column_list_alias->type = INTEGER_LITERAL;
			break;
		case NUMERIC_TYPE:
			cur_column_list_alias->type = NUMERIC_LITERAL;
			break;
		case STRING_TYPE:
			cur_column_list_alias->type = STRING_LITERAL;
			break;
		default:
			assert(FALSE);
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
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
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	assert(NULL != ret);
	return ret;
}
