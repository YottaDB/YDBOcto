/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

static SqlStatement *string_literal(char *name);

SqlStatement *find_column_alias_name(SqlStatement *stmt) {
	SqlColumn	     *column;
	SqlColumnAlias	     *column_alias;
	SqlColumnListAlias   *column_list_alias;
	SqlUnaryOperation    *unary;
	SqlFunctionCall	     *function_call;
	SqlValue	     *value;
	SqlAggregateFunction *aggregate_function;
	SqlStatement	     *ret;
	char		     *c;

	ret = NULL;
	if (NULL == stmt)
		return ret;
	switch (stmt->type) {
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
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
			if (NULL == ret) {
				// Following naming convention is similar to Postgres
				switch (value->u.coerce_type.coerced_type.data_type) {
				case BOOLEAN_TYPE:
					return string_literal("BOOLEAN");
				case INTEGER_TYPE:
					return string_literal("INTEGER");
				case NUMERIC_TYPE:
					return string_literal("NUMERIC");
				case STRING_TYPE:
					return string_literal("VARCHAR");
				case NUL_TYPE:
					assert(FALSE);
				case UNKNOWN_SqlDataType:
					return NULL;
					break; // Avoid compiler warning
				}
			}
			break;
		default:
			// Nothing we can do
			break;
		}
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		if (is_stmt_table_asterisk(column_alias->column)) {
			// Required for processing table.asterisk usage errors by qualify_statement() column_alias_STATEMENT case
			ret = column_alias->column;
		} else if (column_alias->column->type == column_STATEMENT) {
			UNPACK_SQL_STATEMENT(column, column_alias->column, column);
			ret = column->columnName;
		} else {
			UNPACK_SQL_STATEMENT(column_list_alias, column_alias->column, column_list_alias);
			ret = column_list_alias->alias;
		}
		break;
	case binary_STATEMENT:
		ret = NULL;
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		if (BOOLEAN_EXISTS == unary->operation) {
			// Following naming convention is similar to Postgres
			return string_literal("EXISTS");
		} else {
			ret = NULL;
		}
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		ret = find_column_alias_name(function_call->function_name);
		break;
	case coalesce_STATEMENT:
		return string_literal("COALESCE");
	case greatest_STATEMENT:
		return string_literal("GREATEST");
	case least_STATEMENT:
		return string_literal("LEAST");
	case null_if_STATEMENT:
		return string_literal("NULLIF");
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		return string_literal(get_aggregate_func_name(aggregate_function->type));
	case cas_STATEMENT:
		return string_literal("CASE");
		break;
	case column_list_STATEMENT: // this is something like `SELECT 1 IN (...)`
		/* We do not do anything for now */
		break;
	case array_STATEMENT:
		return string_literal("ARRAY");
		break;
	case table_alias_STATEMENT:
	case set_operation_STATEMENT: {
		SqlTableAlias	   *table_alias;
		SqlStatement	   *table_alias_stmt;
		SqlColumnListAlias *cur_cla;

		table_alias_stmt = drill_to_table_alias(stmt);
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(cur_cla, table_alias->column_list, column_list_alias);
		ret = cur_cla->alias;
		break;
	}
	default:
		assert(FALSE);
		break;
	}
	return ret;
}

SqlStatement *string_literal(char *name) {
	SqlStatement *ret;

	SQL_VALUE_STATEMENT(ret, STRING_LITERAL, name);
	return ret;
}
