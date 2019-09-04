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

char *get_type_string(SqlValueType type) {
	switch(type) {
	case NUMBER_LITERAL:
		return "NUMBER";
	case INTEGER_LITERAL:
		return "INTEGER";
	case STRING_LITERAL:
		return "STRING";
	case DATE_TIME:
		return "DATE TIME";
	case TEMPORARY_TABLE_TYPE:
		return "TEMPORARY TABLE TYPE";
	case BOOLEAN_VALUE:
		return "BOOLEAN";
	case PARAMETER_VALUE:
		return "PARAMETER";
	case COLUMN_REFERENCE:
	case CALCULATED_VALUE:
	case UNKNOWN_SqlValueType:
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
	}
	return "";
}

int populate_data_type(SqlStatement *v, SqlValueType *type) {
	SqlValue *value = NULL;
	SqlBinaryOperation *binary = NULL;
	SqlUnaryOperation *unary = NULL;
	SqlTableAlias *table_alias;
	SqlColumn *column = NULL;
	SqlValueType child_type1, child_type2;
	SqlColumnList *cur_list, *start_list;
	SqlColumnListAlias *start_column_list_alias, *cur_column_list_alias;
	SqlFunctionCall *function_call;
	SqlCaseStatement *cas;
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlJoin *start_join, *cur_join;
	YYLTYPE location;
	int result = 0;

	*type = UNKNOWN_SqlValueType;

	if(v == NULL || v->v.select == NULL)
		return 0;

	switch(v->type) {
	case set_operation_STATEMENT:
		*type = TEMPORARY_TABLE_TYPE;
		SqlSetOperation *set_operation;
		UNPACK_SQL_STATEMENT(set_operation, v, set_operation);
		result |= populate_data_type(set_operation->operand[0], &child_type1);
		result |= populate_data_type(set_operation->operand[1], &child_type1);
		break;
	case select_STATEMENT:
		*type = TEMPORARY_TABLE_TYPE;
		SqlSelectStatement *select;
		UNPACK_SQL_STATEMENT(select, v, select);
		result |= populate_data_type(select->select_list, &child_type1);
		result |= populate_data_type(select->table_list, &child_type1);
		result |= populate_data_type(select->where_expression, &child_type1);
		result |= populate_data_type(select->order_expression, &child_type1);
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, v, join);
		cur_join = start_join;
		do {
			result |= populate_data_type(cur_join->value, type);
			result |= populate_data_type(cur_join->condition, type);
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, v, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			result = populate_data_type(value->v.calculated, &child_type1);
			*type = child_type1;
			break;
		case COLUMN_REFERENCE:
			/* if this happens it probably means it wasn't an extended reference
			 * which is not something we want to happen, the parser should expand
			 * all column references to be fully qualified
			 */
			assert(FALSE);
			result = 1;
			break;
		case NUMBER_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case DATE_TIME:
		case PARAMETER_VALUE:
		case UNKNOWN_SqlValueType:
			*type = value->type;
			break;
		case NUL_VALUE:
			*type = UNKNOWN_SqlValueType;
			break;
		case BOOLEAN_VALUE:
			*type = BOOLEAN_VALUE;
			break;
		case COERCE_TYPE:
			*type = value->coerced_type;
			result |= populate_data_type(value->v.coerce_target, &child_type1);
			*v = *value->v.coerce_target;
			if(value_STATEMENT == v->type){
				/* This is a work around the current way COERCE works
				 * It can only currently change the types of literals
				 * It does not work on columns or functions see issue 304
				 */
				if(STRING_LITERAL == v->v.value->type || NUMBER_LITERAL == v->v.value->type || INTEGER_LITERAL == v->v.value->type){
					v->v.value->type = *type;
				}
			}
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, v, binary);
		result = populate_data_type(binary->operands[0], &child_type1);
		result |= populate_data_type(binary->operands[1], &child_type2);
		if(child_type1 == PARAMETER_VALUE || child_type1 == UNKNOWN_SqlValueType) {
			child_type1 = child_type2;
		} else if(child_type2 == PARAMETER_VALUE || child_type2 == UNKNOWN_SqlValueType) {
			child_type2 = child_type1;
		} else if (child_type1 == INTEGER_LITERAL && child_type2 == NUMBER_LITERAL){
			child_type1 = child_type2;
		} else if (child_type2 == INTEGER_LITERAL && child_type1 == NUMBER_LITERAL){
			child_type2 = child_type1;
		}
		switch(binary->operation) {
		case ADDITION:
		case SUBTRACTION:
		case DVISION:
		case MULTIPLICATION:
			*type = child_type1;
			break;
		case CONCAT:
			// Postgres suggests we should force things into a string when we encounter this
			child_type1 = child_type2 = *type = CHARACTER_STRING_TYPE;
			break;
		default:
			*type = BOOLEAN_VALUE;
			break;
		}
		if(child_type1 != child_type2 && child_type2 != TEMPORARY_TABLE_TYPE) {
			WARNING(ERR_TYPE_MISMATCH, get_type_string(child_type1), get_type_string(child_type2));
			location = binary->operands[0]->loc;
			location.last_line = binary->operands[1]->loc.last_line;
			location.last_column = binary->operands[1]->loc.last_column;
			yyerror(&location, NULL, NULL, NULL, "Type mismatch");
			result = 1;
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
			result |= populate_data_type(cur_list->value, type);
			cur_list = cur_list->next;
		} while(start_list != cur_list);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_column_list_alias, v, column_list_alias);
		cur_column_list_alias = start_column_list_alias;
		do {
			/// TODO: there is a high chance of a bug here if data types occur at different nested errors
			result |= populate_data_type(cur_column_list_alias->column_list, &child_type1);
			cur_column_list_alias->type = child_type1;
			if(*type == UNKNOWN_SqlValueType) {
				*type = child_type1;
			} else if (*type != child_type1) {
				//result |= 1;
			}
			cur_column_list_alias = cur_column_list_alias->next;
		} while(cur_column_list_alias != start_column_list_alias);
		break;
	case column_alias_STATEMENT:
		if(v->v.column_alias->column->type == column_list_alias_STATEMENT) {
			populate_data_type(v->v.column_alias->column, type);
		} else {
			UNPACK_SQL_STATEMENT(column, v->v.column_alias->column, column);
			switch(column->type) {
			case CHARACTER_STRING_TYPE:
				*type = STRING_LITERAL;
				break;
			case NUMERIC_TYPE:
				*type = NUMBER_LITERAL;
				break;
			case INTEGER_TYPE:
				*type = INTEGER_LITERAL;
				break;
			case DATE_TIME_TYPE:
				*type = DATE_TIME;
				break;
			case INTERVAL_TYPE:
			case UNKNOWN_SqlDataType:
			default:
				ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
				result = 1;
			}
		}
		break;
	case function_call_STATEMENT:
		// TODO: we will need to know the return types of functions to do this
		// For now, just say STRING_LITERAL
		UNPACK_SQL_STATEMENT(function_call, v, function_call);
		populate_data_type(function_call->parameters, type);
		*type = STRING_LITERAL;
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, v, cas);
		// We expect type to get overriden here; only the last type matters
		result |= populate_data_type(cas->value, type);
		result |= populate_data_type(cas->branches, type);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, v, cas_branch);
		cur_branch = cas_branch;
		do {
			result |= populate_data_type(cur_branch->condition, type);
			/// TODO: we should check type here to make sure all branches have the
			// same type
			result |= populate_data_type(cur_branch->value, type);
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, v, table_alias);
		result |= populate_data_type(table_alias->table, type);
		break;
	case table_STATEMENT:
		// Do nothing; we got here through a table_alias
		break;
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		result = 1;
	}
	return result;
}
