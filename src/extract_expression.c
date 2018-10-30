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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"

/**
 * WARNING: caller is responsible for freeing the buffer
 */
char *extract_expression(ydb_buffer_t *cursor_global,
                         ydb_buffer_t *cursor_exe_global,
                         SqlStatement *stmt, const SqlTable *table, char *source)
{
	SqlStatement *calculated;
	SqlBinaryOperation *binary;
	SqlValue *value;
	SqlTable *sub_table;
	char *buffer = malloc(MAX_EXPRESSION_LENGTH);
	char *tmp1 = NULL,  *tmp2 = NULL, *tmp3 = NULL;

	switch(stmt->type)
	{
	case select_STATEMENT:
		buffer[0] = '\0'; // this is a special case hopefully only seen by IN
		break;
	case value_STATEMENT:
		value = stmt->v.value;
		switch(value->type)
		{
		case STRING_LITERAL:
			snprintf(buffer, MAX_EXPRESSION_LENGTH, "\"%s\"", value->v.string_literal);
			break;
		case NUMBER_LITERAL:
			snprintf(buffer, MAX_EXPRESSION_LENGTH, "%s", value->v.string_literal);
			break;
		case COLUMN_REFERENCE:
			emit_simple_select(buffer, table, value->v.reference, source);
			break;
		case CALCULATED_VALUE:
			calculated = value->v.calculated;
			switch(calculated->type)
			{
			case value_STATEMENT:
				tmp1 = extract_expression(cursor_global, cursor_exe_global, calculated, table, source);
				snprintf(buffer, MAX_EXPRESSION_LENGTH, "%s", tmp1);
				break;
			case binary_STATEMENT:
				free(buffer);
				buffer = extract_expression(cursor_global, cursor_exe_global, calculated, table, source);
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
		tmp1 = extract_expression(cursor_global, cursor_exe_global, binary->operands[0], table, source);
		tmp2 = extract_expression(cursor_global, cursor_exe_global, binary->operands[1], table, source);
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
			tmp3 = "=";
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
			/* This is a somewhat special case since we need to evaluate the
			    temporary table passed as the second argument */
			sub_table = emit_xref_table(cursor_global, cursor_exe_global, binary->operands[1]);
			// The generated table should be a single column table with the primary key
			//  being the only column
			UNPACK_SQL_STATEMENT(value, sub_table->tableName, value);
			snprintf(buffer, MAX_EXPRESSION_LENGTH, "($D(^%s(%s))'=0)",
			         value->v.reference, tmp1);
			 free(tmp1);
			 free(tmp2);
			return buffer;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		snprintf(buffer, MAX_EXPRESSION_LENGTH, "(%s%s%s)", tmp1, tmp3, tmp2);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}
	if(tmp1)
		free(tmp1);
	if(tmp2)
		free(tmp2);
	return buffer;
}
