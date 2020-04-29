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

// Function invoked by the rule named "table_reference" in src/parser/select.y
SqlStatement *table_reference(SqlStatement *column_name, SqlStatement *correlation_specification,
							SqlStatement *table_reference_tail, int *plan_id)
{
	SqlStatement	*ret;
	SqlJoin		*join, *join_tail;
	SqlTable	*table;
	SqlColumn	*column;
	SqlTableAlias	*alias;

	table = find_table(column_name->v.value->v.reference);
	if (NULL == table) {
		ERROR(ERR_UNKNOWN_TABLE, column_name->v.value->v.reference);
		yyerror(NULL, NULL, &column_name, NULL, NULL, NULL);
		return NULL;
	}
	SQL_STATEMENT(ret, join_STATEMENT);
	MALLOC_STATEMENT(ret, join, SqlJoin);
	join = ret->v.join;
	SQL_STATEMENT(join->value, table_alias_STATEMENT);
	MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
	SQL_STATEMENT_FROM_TABLE_STATEMENT(alias->table, table);
	alias->table->v.create_table = table;
	alias->alias = ((NULL != correlation_specification) ? correlation_specification : table->tableName);
	// We can probably put a variable in the bison local for this
	alias->unique_id = *plan_id;
	(*plan_id)++;
	UNPACK_SQL_STATEMENT(column, table->columns, column);
	PACK_SQL_STATEMENT(alias->column_list, columns_to_column_list_alias(column, join->value), column_list_alias);
	dqinit(join);
	if (NULL != table_reference_tail) {
		UNPACK_SQL_STATEMENT(join_tail, table_reference_tail, join);
		join->type = CROSS_JOIN;
		dqappend(join, join_tail);
	}
	return ret;
}
