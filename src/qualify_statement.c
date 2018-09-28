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

int qualify_statement(SqlStatement *stmt, SqlJoin *tables) {
	SqlUnaryOperation *unary;
	SqlBinaryOperation *binary;
	SqlValue *value;
	int result = 0, column_found = 0;

	if(stmt == NULL)
		return 0;

	switch(stmt->type) {
	case select_STATEMENT:
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case COLUMN_REFERENCE:
			column_found = qualify_column_name(&value->v.reference, tables) == 0;
			if(!column_found) {
				print_yyloc(&stmt->loc);
			}
			result |= !column_found;
			break;
		case CALCULATED_VALUE:
			result |= qualify_statement(value->v.calculated, tables);
			break;
		default:
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		result |= qualify_statement(binary->operands[0], tables);
		result |= qualify_statement(binary->operands[1], tables);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		result |= qualify_statement(unary->operand, tables);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}
	return result;
}
