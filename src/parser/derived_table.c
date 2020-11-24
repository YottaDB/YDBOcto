/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by the rule named "derived_table" in src/parser/select.y
 *
 * Returns:
 *	non-NULL pointer in case of success.
 *	NULL     pointer in case of errors so caller can take appropriate action.
 */
SqlStatement *derived_table(SqlStatement *table_subquery, SqlStatement *correlation_specification) {
	SqlStatement *sql_stmt, *ret;
	SqlJoin *     join;

	SQL_STATEMENT(ret, join_STATEMENT);
	MALLOC_STATEMENT(ret, join, SqlJoin);
	join = ret->v.join;
	sql_stmt = table_subquery;
	join->value = sql_stmt;
	if (NULL != correlation_specification) {
		SqlTableAlias *table_alias;
		SqlColumnList *column_list;

		// Setup the alias
		sql_stmt = drill_to_table_alias(sql_stmt);
		UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
		UNPACK_SQL_STATEMENT(column_list, correlation_specification, column_list);
		table_alias->alias = column_list->value;
		table_alias->correlation_specification = correlation_specification;
		/* Defer processing column name aliases till all asterisk or table.* usage is expanded in qualify_query() */
	}
	dqinit(join);
	join->max_unique_id = config->plan_id;
	return ret;
}
