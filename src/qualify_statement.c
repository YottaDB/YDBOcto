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

int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *column_list_alias, boolean_t match_qualified_columns) {
	SqlUnaryOperation *unary;
	SqlBinaryOperation *binary;
	SqlFunctionCall *fc;
	SqlColumnList *start_cl, *cur_cl, *column_list;
	SqlValue *value;
	SqlCaseStatement *cas;
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlColumnListAlias *start_cla, *cur_cla;
	int result;

	if(stmt == NULL)
		return 0;

	result = 0;

	switch(stmt->type) {
	case select_STATEMENT:
		assert(FALSE);
		break;
	case column_alias_STATEMENT:
		// We can get here if the select list was empty and we took
		//  all columns from the table
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case COLUMN_REFERENCE:
			/// TODO: the value is being leaked here
			stmt->v.column_alias = qualify_column_name(value, tables, column_list_alias, match_qualified_columns);
			result |= stmt->v.column_alias == NULL;
			if(result) {
				print_yyloc(&stmt->loc);
			}
			// Convert this statement to a qualified one.
			// Note: "match_column_in_table.c" and "qualify_column_name.c" rely on the below for qualifying
			//       column names (in case MATCH_QUALIFIED_COLUMNS_TRUE is passed in to those functions.
			stmt->type = column_alias_STATEMENT;
			break;
		case CALCULATED_VALUE:
			result |= qualify_statement(value->v.calculated, tables, column_list_alias, match_qualified_columns);
			break;
		case FUNCTION_NAME:
			// If it starts with '$$', trim those off and leave it alone (MUMPS expression)
			// Else, match it with a value from the dictionary in ^octo("functions")
			result = qualify_function_name(stmt);
			break;
		case COERCE_TYPE:
			result |= qualify_statement(value->v.coerce_target, tables, column_list_alias, match_qualified_columns);
			break;
		default:
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		result |= qualify_statement(binary->operands[0], tables, column_list_alias, match_qualified_columns);
		result |= qualify_statement(binary->operands[1], tables, column_list_alias, match_qualified_columns);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		result |= qualify_statement(unary->operand, tables, column_list_alias, match_qualified_columns);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		UNPACK_SQL_STATEMENT(column_list, fc->parameters, column_list);
		// TODO: qualify function name?
		result |= qualify_statement(fc->function_name, tables, column_list_alias, match_qualified_columns);
		//result |= qualify_statement(fc->function_name, tables);
		result |= qualify_column_list(column_list, tables, column_list_alias, match_qualified_columns);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		result |= qualify_statement(cas->value, tables, column_list_alias, match_qualified_columns);
		result |= qualify_statement(cas->branches, tables, column_list_alias, match_qualified_columns);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		do {
			result |= qualify_statement(cur_branch->condition, tables, column_list_alias, match_qualified_columns);
			result |= qualify_statement(cur_branch->value, tables, column_list_alias, match_qualified_columns);
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case column_list_STATEMENT:
		// This is a result of a value-list
		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			result |= qualify_statement(cur_cl->value, tables, column_list_alias, match_qualified_columns);
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		result |= qualify_query(stmt, tables);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		cur_cla = start_cla;
		do {
			result |= qualify_statement(cur_cla->column_list, tables, column_list_alias, match_qualified_columns);
			cur_cla = cur_cla->next;
		} while(cur_cla != start_cla);
		break;
	case table_STATEMENT:
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		break;
	}
	return result;
}
