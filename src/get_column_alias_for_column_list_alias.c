/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

/**
 * Checks if a SqlColumnAlias already exists for a SqlColumnListAlias structure. If so returns that.
 * If not allocates a SqlColumnAlias and returns that.
 */
SqlColumnAlias *get_column_alias_for_column_list_alias(SqlColumnListAlias *col_cla, SqlStatement *matching_alias_stmt) {
	SqlColumnList * col_list;
	SqlColumnAlias *ret;
	SqlTableAlias * table_alias, *matching_alias;

	UNPACK_SQL_STATEMENT(matching_alias, matching_alias_stmt, table_alias);
	/* Check if "col_cla" already points to a SqlColumnAlias that we can return.
	 * If so, use that instead of allocating a new one.
	 */
	if (NULL != col_cla->outer_query_column_alias) {
		/* We already allocated a column_alias for this column_list_alias. Return that. */
		assert((table_alias_STATEMENT == col_cla->outer_query_column_alias->v.column_alias->table_alias_stmt->type)
		       && (matching_alias == col_cla->outer_query_column_alias->v.column_alias->table_alias_stmt->v.table_alias));
		return col_cla->outer_query_column_alias->v.column_alias;
	}
	UNPACK_SQL_STATEMENT(col_list, col_cla->column_list, column_list);
	if (column_alias_STATEMENT == col_list->value->type) {
		UNPACK_SQL_STATEMENT(ret, col_list->value, column_alias);
		/* Note: ret can be NULL in case of a parse error. Hence the need to handle this case below */
		if (NULL != ret) {
			UNPACK_SQL_STATEMENT(table_alias, ret->table_alias_stmt, table_alias);
			if (matching_alias != table_alias) {
				/* table_alias not matching means for example that the column in a sub-query is being
				 * referenced/inherited in a parent-query. Need to allocate a new SqlColumnAlias for
				 * `matching_table_alias`.
				 */
				ret = NULL;
			}
		}
	} else {
		ret = NULL;
	}
	if (NULL == ret) {
		OCTO_CMALLOC_STRUCT(ret, SqlColumnAlias);
		PACK_SQL_STATEMENT(ret->column, col_cla, column_list_alias);
		ret->table_alias_stmt = matching_alias_stmt;
	}
	/* Store this column_alias for faster returns from `qualify_column_name.c` for same col_cla */
	SQL_STATEMENT(col_cla->outer_query_column_alias, column_alias_STATEMENT);
	col_cla->outer_query_column_alias->v.column_alias = ret;
	return ret;
}
