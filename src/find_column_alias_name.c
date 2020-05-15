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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"

SqlStatement *find_column_alias_name(SqlStatement *stmt) {
	SqlColumn		*column;
	SqlColumnAlias		*column_alias;
	SqlColumnListAlias	*column_list_alias;
	SqlBinaryOperation	*binary;
	SqlUnaryOperation	*unary;
	SqlFunctionCall		*function_call;
	SqlValue		*value;
	SqlAggregateFunction	*aggregate_function;
	SqlStatement		*ret;
	SqlCaseStatement	*cas;
	char			*c;

	ret = NULL;
	if (NULL == stmt)
		return ret;
	switch(stmt->type) {
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case COLUMN_REFERENCE:
			ret = copy_sql_statement(stmt);
			c = ret->v.value->v.string_literal;
			// Strip off any leading 'table.' stuff, since that shouldn't be propagated
			while ('\0' != *c) {
				if ('.' == *c) {
					break;
				}
				c++;
			}
			if (*c == '.') {
				ret->v.value->v.string_literal = c + 1;
			}
			ret->v.value->type = STRING_LITERAL;
			break;
		case CALCULATED_VALUE:
			ret = find_column_alias_name(value->v.calculated);
			break;
		case FUNCTION_NAME:
			ret = copy_sql_statement(stmt);
			c = ret->v.value->v.string_literal;
			// Strip off any leading '$$' stuff in the function name
			while ('$' == *c) {
				c++;
			}
			ret->v.value->v.string_literal = c;
			// Strip off any '^%' usage in the function name.
			// For example, "$$ABS^%ydboctosqlfunctions" should translate to "ABS" as the column name)
			while (('^' != *c) && ('\0' != *c)) {
				c++;
			}
			*c = '\0';
			ret->v.value->type = STRING_LITERAL;
			break;
		case COERCE_TYPE:
			ret = find_column_alias_name(value->v.coerce_target);
			break;
		default:
			// Nothing we can do
			break;
		}
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		if (column_alias->column->type == column_STATEMENT) {
			UNPACK_SQL_STATEMENT(column, column_alias->column, column);
			ret = column->columnName;
		} else {
			UNPACK_SQL_STATEMENT(column_list_alias, column_alias->column, column_list_alias);
			ret = column_list_alias->alias;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		ret = find_column_alias_name(binary->operands[0]);
		if (NULL != ret)
			break;
		ret = find_column_alias_name(binary->operands[1]);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		ret = find_column_alias_name(unary->operand);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		ret = find_column_alias_name(function_call->function_name);
		break;
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		SQL_STATEMENT(ret, value_STATEMENT);
		OCTO_CMALLOC_STRUCT(ret->v.value, SqlValue);
		ret->v.value->type = STRING_LITERAL;
		ret->v.value->v.string_literal = get_aggregate_func_name(aggregate_function->type);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		if (NULL != cas->value) {
			ret = find_column_alias_name(cas->value);
		} else {
			SQL_STATEMENT(ret, value_STATEMENT);
			OCTO_CMALLOC_STRUCT(ret->v.value, SqlValue);
			ret->v.value->type = STRING_LITERAL;
			ret->v.value->v.string_literal = "CASE";
		}
		break;
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		/* We do not do anything for now */
		break;
	default:
		assert(FALSE);
		break;
	}
	return ret;
}
