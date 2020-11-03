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
#include "octo_types.h"

// Function invoked by the rule named "insert_statement" in src/parser/insert.y
SqlStatement *insert_statement(SqlStatement *table_name, SqlStatement *column_name_list, SqlStatement *query_expression,
			       int *plan_id, ParseContext *parse_context) {
	SqlStatement *	    ret, *validated_query_expression;
	SqlStatement *	    join_stmt;
	SqlInsertStatement *insert;
	SqlJoin *	    join;
	SqlTableAlias *	    table_alias;

	assert(value_STATEMENT == table_name->type);
	join_stmt = table_reference(table_name, NULL, plan_id);
	if (NULL == join_stmt) {
		return NULL;
	}
	validated_query_expression = validate_query_expression(query_expression, parse_context);
	if (NULL == validated_query_expression) {
		return NULL;
	}
	assert(validated_query_expression == query_expression);
	SQL_STATEMENT(ret, insert_STATEMENT);
	MALLOC_STATEMENT(ret, insert, SqlInsertStatement);
	UNPACK_SQL_STATEMENT(insert, ret, insert);
	UNPACK_SQL_STATEMENT(join, join_stmt, join);
	UNPACK_SQL_STATEMENT(table_alias, join->value, table_alias);
	/* All we want here is "table_alias->table" but we need "table_alias" in order to later store this as a LP_TABLE
	 * logical plan. Hence the call to "table_reference()" above. Otherwise a call to "find_table()" would have sufficed.
	 */
	insert->dst_table_alias = table_alias;
	insert->columns = column_name_list;
	insert->src_table_alias_stmt = validated_query_expression;
	return ret;
}
