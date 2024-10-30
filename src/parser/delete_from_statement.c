/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by the rule named "delete_statement_searched" in src/delete.y
 * Returns
 *	non-NULL in case of success
 *	NULL     in case of error
 */
SqlStatement *delete_from_statement(SqlStatement *table_name, SqlStatement *alias_name, SqlStatement *where_clause, int *plan_id,
				    ParseContext *parse_context) {
	SqlStatement  *join_stmt;
	SqlJoin	      *join;
	SqlTableAlias *table_alias;
	SqlTable      *table;

	assert(value_STATEMENT == table_name->type);
	join_stmt = table_reference(table_name, NULL, plan_id, TRUE);
	if (NULL == join_stmt) {
		return NULL;
	}
	UNPACK_SQL_STATEMENT(join, join_stmt, join);
	UNPACK_SQL_STATEMENT(table_alias, join->value, table_alias);
	/* Untill DELETE FROM is allowed with Views (YDBOcto#924) generate an error if
	 * the table_name corresponds to a view.
	 */
	IF_VIEW_ISSUE_UNSUPPORTED_OPERATION_ERROR(table_alias->table, delete_from_STATEMENT);
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	if (!table->readwrite) {
		ERROR(ERR_TABLE_READONLY, "DELETE", table_name->v.value->v.reference);
		return NULL;
	}
	table_alias->alias = ((NULL != alias_name) ? alias_name : table->tableName);

	SqlDeleteFromStatement *delete;
	SqlStatement *ret, *validated_query_expression;

	SQL_STATEMENT(ret, delete_from_STATEMENT);
	MALLOC_STATEMENT(ret, delete_from, SqlDeleteFromStatement);
	UNPACK_SQL_STATEMENT(delete, ret, delete_from);
	delete->src_join = join_stmt;
	delete->where_clause = where_clause;
	validated_query_expression = validate_query_expression(ret, parse_context, delete_from_STATEMENT);
	return validated_query_expression;
}
