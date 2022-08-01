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

// Function invoked by the rule named "sort_specification" in src/parser/parser.y
// When supported collate clause SqlStatement may be passed here.
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *ordering_specification) {
	SqlStatement *	    ret, *order_spec;
	SqlColumnListAlias *alias;

	SQL_COLUMN_LIST_ALIAS_STATEMENT(ret);
	UNPACK_SQL_STATEMENT(alias, ret, column_list_alias);
	switch (sort_key->type) {
	case value_STATEMENT:;
		SqlValue *value;
		UNPACK_SQL_STATEMENT(value, sort_key, value);
		switch (value->type) {
		case INTEGER_LITERAL:
		case NUMERIC_LITERAL:
		case TABLE_ASTERISK:
		case COLUMN_REFERENCE:
		case BOOLEAN_VALUE:
		case COERCE_TYPE:
		case CALCULATED_VALUE:
			// No error
			break;
		case STRING_LITERAL:
		case BOOLEAN_OR_STRING_LITERAL:
		case NUL_VALUE:
		case PARAMETER_VALUE:
			/* Handle `PARAMETER_VALUE` type here as `$1` in the following type of usages can reach this code.
			 * `SELECT 1 FROM names GROUP BY 1 ORDER BY $1;`
			 * This usage is not allowed. Issue error.
			 */
			ERROR(ERR_ORDER_BY_POSITION_NOT_INTEGER, "", value->v.reference)
			yyerror(NULL, NULL, &(sort_key), NULL, NULL, NULL);
			return NULL;
		case FUNCTION_NAME:
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
			assert(FALSE);
			break;
		}
	default:
		break;
	}
	alias->column_list = create_sql_column_list(sort_key, NULL, &sort_key->loc);
	// Add a keyword for ASC or DESC. Default to ASC if not explicitly specified.
	OptionalKeyword order_spec_type
	    = ((NULL == ordering_specification) || ((SqlStatement *)OPTIONAL_ASC == ordering_specification)) ? OPTIONAL_ASC
													     : OPTIONAL_DESC;
	MALLOC_KEYWORD_STMT(order_spec, order_spec_type);
	order_spec->v.keyword->v = NULL;
	alias->keywords = order_spec;
	return ret;
}
