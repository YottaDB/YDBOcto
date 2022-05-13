/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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
	SqlColumnListAlias *alias;
	SqlStatement *	    ret;
	SqlValue *	    value;
	boolean_t	    invalid_syntax;

	assert(NULL == collate_clause); /* below code needs to be fixed to handle collate_clause if/when it becomes non-NULL */
	UNUSED(collate_clause);
	switch (derived_column_expression->type) {
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, derived_column_expression, value);
		switch (value->type) {
		case INTEGER_LITERAL:
		case COLUMN_REFERENCE:
		case TABLE_ASTERISK:
		case BOOLEAN_VALUE:
		case COERCE_TYPE:
			invalid_syntax = FALSE;
			break;
		case CALCULATED_VALUE:;
			SqlStatement *calculated_value = value->v.calculated;
			switch (calculated_value->type) {
			case function_call_STATEMENT:
			case coalesce_STATEMENT:
			case null_if_STATEMENT:
			case greatest_STATEMENT:
			case least_STATEMENT:
				invalid_syntax = FALSE;
				break;
			default:
				invalid_syntax = TRUE;
				break;
			}
			break;
		default:
			invalid_syntax = TRUE;
			break;
		}
		break;
	case binary_STATEMENT:
	case unary_STATEMENT:
	case cas_STATEMENT:
		invalid_syntax = FALSE;
		break;
	default:
		invalid_syntax = TRUE;
		break;
	}
	if (invalid_syntax)
		return NULL;

	SQL_COLUMN_LIST_ALIAS_STATEMENT(ret);
	UNPACK_SQL_STATEMENT(alias, ret, column_list_alias);
	alias->column_list = create_sql_column_list(derived_column_expression, NULL, &derived_column_expression->loc);
	return ret;
}
