/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/**
 * Changes where to be optimized, returns the number of optimizations that took place
 *
 * May overwrite values in join to replace cursors in destination tables
 */
int optimize_where(SqlStatement *stmt, SqlJoin *join) {
	SqlStatement *calculated;
	SqlBinaryOperation *binary;
	SqlValue *value;
	SqlTable *sub_table;
	char *buffer = malloc(MAX_EXPRESSION_LENGTH);
	char *tmp1 = NULL,  *tmp2 = NULL, *tmp3 = NULL;
	int result = 0;

	if(stmt == NULL)
		return 0;

	switch(stmt->type)
	{
	case select_STATEMENT:
		break;
	case value_STATEMENT:
		value = stmt->v.value;
		switch(value->type)
		{
		case STRING_LITERAL:
			break;
		case NUMBER_LITERAL:
			break;
		case COLUMN_REFERENCE:
			break;
		case CALCULATED_VALUE:
			calculated = value->v.calculated;
			switch(calculated->type)
			{
			case value_STATEMENT:
			case binary_STATEMENT:
				result = optimize_where(calculated, join);
				break;
			default:
				FATAL(ERR_UNKNOWN_KEYWORD_STATE);
				break;
			}
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		binary = stmt->v.binary;
		result += optimize_where(binary->operands[0], join);
		result += optimize_where(binary->operands[1], join);
		switch(binary->operation)
		{
		case ADDITION:
			tmp3 = "+";
			break;
		case SUBTRACTION:
			tmp3 = "-";
			break;
		case DVISION:
			tmp3 = "/";
			break;
		case MULTIPLICATION:
			tmp3 = "*";
			break;
		case CONCAT:
			tmp3 = "_";
			break;
		case BOOLEAN_OR:
			tmp3 = "!";
			break;
		case BOOLEAN_AND:
			tmp3 = "&";
			break;
		case BOOLEAN_IS:
			tmp3 = "_";
			FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "BOOLEAN_IS");
			break;
		case BOOLEAN_EQUALS:
			result = constant_equal_column(stmt, join);
			break;
		case BOOLEAN_NOT_EQUALS:
			tmp3 = "'=";
			break;
		case BOOLEAN_LESS_THAN:
			tmp3 = "<";
			break;
		case BOOLEAN_GREATER_THAN:
			tmp3 = ">";
			break;
		case BOOLEAN_LESS_THAN_OR_EQUALS:
			tmp3 = "'>'";
			break;
		case BOOLEAN_GREATER_THAN_OR_EQUALS:
			tmp3 = "'<'";
			break;
		case BOOLEAN_IN:
		case BOOLEAN_NOT_IN:
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}
	return result;
}
