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

int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *column_list_alias) {
	SqlUnaryOperation *unary;
	SqlBinaryOperation *binary;
	SqlFunctionCall *fc;
	SqlColumnList *start_cl, *cur_cl, *column_list;
	SqlValue *value;
	SqlCaseStatement *cas;
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlTableAlias *table_alias;
	int result;

	if(stmt == NULL)
		return 0;

	result = 0;

	switch(stmt->type) {
	case select_STATEMENT:
		break;
	case column_alias_STATEMENT:
		// We can get here if the select list was empty and we took
		//  all columns from the table
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case COLUMN_REFERENCE:
			// Convert this statement to a qualified one
			stmt->type = column_alias_STATEMENT;
			/// TODO: the value is being leaked here
			stmt->v.column_alias = qualify_column_name(value, tables, column_list_alias);
			result |= stmt->v.column_alias == NULL;
			if(result) {
				print_yyloc(&stmt->loc);
			}
			break;
		case CALCULATED_VALUE:
			result |= qualify_statement(value->v.calculated, tables, column_list_alias);
			break;
		case FUNCTION_NAME:
			// If it starts with '$$', trim those off and leave it alone (MUMPS expression)
			// Else, match it with a value from the dictionary in ^octo("functions")
			result = qualify_function_name(stmt);
			break;
		default:
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		result |= qualify_statement(binary->operands[0], tables, column_list_alias);
		result |= qualify_statement(binary->operands[1], tables, column_list_alias);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		result |= qualify_statement(unary->operand, tables, column_list_alias);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		UNPACK_SQL_STATEMENT(column_list, fc->parameters, column_list);
		// TODO: qualify function name?
		result |= qualify_statement(fc->function_name, tables, column_list_alias);
		//result |= qualify_statement(fc->function_name, tables);
		result |= qualify_column_list(column_list, tables, column_list_alias);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		result |= qualify_statement(cas->value, tables, column_list_alias);
		result |= qualify_statement(cas->branches, tables, column_list_alias);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		do {
			result |= qualify_statement(cur_branch->condition, tables, column_list_alias);
			result |= qualify_statement(cur_branch->value, tables, column_list_alias);
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case column_list_STATEMENT:
		// This is a result of a value-list
		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			result |= qualify_statement(cur_cl->value, tables, column_list_alias);
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		result |= qualify_statement(table_alias->table, tables, column_list_alias);
		/// TODO: this should be qualified through a different recursion path
		// result |= qualify_statement(table_alias->column_list, tables, column_list_alias);
		break;
	case table_STATEMENT:
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		break;
	}
	return result;
}
