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

// Function invoked by the rule named "derived_table" in src/parser/select.y
SqlStatement *derived_table(SqlStatement *table_subquery, SqlStatement *correlation_specification,
			    SqlStatement *table_reference_tail) {
	SqlStatement *sql_stmt, *ret;
	SqlJoin *     join;

	SQL_STATEMENT(ret, join_STATEMENT);
	MALLOC_STATEMENT(ret, join, SqlJoin);
	join = ret->v.join;
	sql_stmt = table_subquery;
	join->value = sql_stmt;
	if (NULL != correlation_specification) {
		SqlTableAlias *table_alias;

		// Setup the alias
		sql_stmt = drill_to_table_alias(sql_stmt);
		UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
		table_alias->alias = correlation_specification;
	}
	dqinit(join);
	if (NULL != table_reference_tail) {
		SqlJoin *join_tail;

		UNPACK_SQL_STATEMENT(join_tail, table_reference_tail, join);
		join_tail->type = CROSS_JOIN;
		dqappend(join, join_tail);
	}
	return ret;
}
