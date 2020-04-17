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

// Function invoked by the rule named "grouping_column_reference" in src/parser/select.y
SqlStatement *grouping_column_reference(SqlStatement *derived_column_expression, SqlStatement *collate_clause) {
	SqlColumnList		*column_list;
	SqlColumnListAlias	*alias;
	SqlStatement		*ret;
	SqlValue		*value;
	boolean_t		invalid_syntax;

	assert(NULL == collate_clause);	/* below code needs to be fixed to handle collate_clause if/when it becomes non-NULL */
	UNUSED(collate_clause);
	/* If it is a value check if it is a column name. Otherwise return NULL so caller (select.y) can issue an error.
	 * If it is not a value then it is some kind of expression so return NULL so caller (select.y) can issue an error.
	 */
	if (value_STATEMENT == derived_column_expression->type) {
		UNPACK_SQL_STATEMENT(value, derived_column_expression, value);
		invalid_syntax = (COLUMN_REFERENCE != value->type);
	} else {
		invalid_syntax = TRUE;
	}
	if (invalid_syntax)
		return NULL;

	SQL_STATEMENT(ret, column_list_alias_STATEMENT);
	MALLOC_STATEMENT(ret, column_list_alias, SqlColumnListAlias);
	UNPACK_SQL_STATEMENT(alias, ret, column_list_alias);
	SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
	dqinit(alias);
	MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
	UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
	dqinit(column_list);
	column_list->value = derived_column_expression;
	alias->column_list->loc = derived_column_expression->loc;	/* Cannot use "yyloc" here so passing it from parser
									 * through derived_column_expression->loc.
									 */
	return ret;
}
