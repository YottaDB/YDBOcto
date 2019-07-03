/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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
	SqlColumn *column;
	SqlColumnAlias *column_alias;
	SqlColumnListAlias *column_list_alias;
	SqlBinaryOperation *binary;
	SqlUnaryOperation *unary;
	SqlValue *value;
	SqlStatement *ret = NULL;
	if(stmt == NULL)
		return ret;
	switch(stmt->type) {
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case COLUMN_REFERENCE:
			ret = copy_sql_statement(stmt);
			// Strip off any leading 'table.' stuff, since that shouldn't
			// be propgated
			char *c = ret->v.value->v.string_literal;
			while(*c != '\0') {
				if(*c == '.') {
					break;
				}
				c++;
			}
			if(*c == '.') {
				ret->v.value->v.string_literal = c + 1;
			}
			ret->v.value->type = STRING_LITERAL;
			break;
		case CALCULATED_VALUE:
			ret = find_column_alias_name(value->v.calculated);
			break;
		default:
			// Nothing we can do
			break;
		}
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		if(column_alias->column->type == column_STATEMENT) {
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
		if(ret != NULL)
			break;
		ret = find_column_alias_name(binary->operands[1]);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		ret = find_column_alias_name(unary->operand);
		break;
	default:
		// Nothing we can do here
		break;
	}
	return ret;
}
