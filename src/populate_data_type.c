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
#include "octo_types.h"

char *get_type_string(SqlValueType type) {
	switch(type) {
	case NUMBER_LITERAL:
		return "NUMBER";
		break;
	case STRING_LITERAL:
		return "STRING";
		break;
	case DATE_TIME:
		return "DATE TIME";
		break;
	case TEMPORARY_TABLE_TYPE:
		return "TEMPORARY TABLE TYPE";
		break;
	case COLUMN_REFERENCE:
	case CALCULATED_VALUE:
	case UNKNOWN_SqlValueType:
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
	}
	return "";
}

int populate_data_type(SqlStatement *v, SqlValueType *type) {
	SqlValue *value = NULL;
	SqlBinaryOperation *binary = NULL;
	SqlUnaryOperation *unary = NULL;
	SqlTable *table = NULL;
	SqlColumn *column = NULL;
	SqlValueType child_type1, child_type2;
	SqlColumnList *cur_list, *start_list;
	YYLTYPE location;
	int result = 0;
	char *c = NULL;

	*type = UNKNOWN_SqlValueType;

	if(v == NULL)
		return 0;

	switch(v->type) {
	case select_STATEMENT:
		*type = TEMPORARY_TABLE_TYPE;
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, v, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			result = populate_data_type(value->v.calculated, &child_type1);
			*type = child_type1;
			break;
		case COLUMN_REFERENCE:
			c = value->v.reference;
			for(; *c != '.' && *c != '\0'; c++) {
				// Empty
			}
			assert(*c != '\0');
			*c = '\0';
			table = find_table(value->v.reference);
			*c++ = '.';
			/* if this happens it probably means it wasn't an extended reference
			 * which is not something we want to happen, the parser should expand
			 * all column references to be fully qualified
			 */
			if(table == NULL) {
				ERROR(ERR_UNKNOWN_TABLE, c);
				print_yyloc(&v->loc);
				return 1;
			}
			column = find_column(c, table);
			switch(column->type) {
			case CHARACTER_STRING_TYPE:
				*type = STRING_LITERAL;
				break;
			case INTEGER_TYPE:
				*type = NUMBER_LITERAL;
				break;
			case DATE_TIME_TYPE:
				*type = DATE_TIME;
			case INTERVAL_TYPE:
			case UNKNOWN_SqlDataType:
			default:
				FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			}
			break;
		case NUMBER_LITERAL:
		case STRING_LITERAL:
		case DATE_TIME:
			*type = value->type;
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, v, binary);
		result = populate_data_type(binary->operands[0], &child_type1);
		result |= populate_data_type(binary->operands[1], &child_type2);
		*type = child_type1;
		if(child_type1 != child_type2 && child_type2 != TEMPORARY_TABLE_TYPE) {
			WARNING(ERR_TYPE_MISMATCH, get_type_string(child_type1), get_type_string(child_type2));
			location = binary->operands[0]->loc;
			location.last_line = binary->operands[1]->loc.last_line;
			location.last_column = binary->operands[1]->loc.last_column;
			print_yyloc(&location);
			return 1;
		}
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, v, unary);
		result = populate_data_type(unary->operand, &child_type1);
		*type = child_type1;
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_list, v, column_list);
		cur_list = start_list;
		do {
			result |= populate_data_type(cur_list->value, &child_type1);
			cur_list = cur_list->next;
		} while(start_list != cur_list);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
	}
	return result;
}
