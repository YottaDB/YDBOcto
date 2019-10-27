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

char	*get_setoperation_string(SqlSetOperationType type) {
	switch(type) {
	case SET_UNION:
		return "UNION";
		break;
	case SET_UNION_ALL:
		return "UNION ALL";
		break;
	case SET_EXCEPT:
		return "EXCEPT";
		break;
	case SET_EXCEPT_ALL:
		return "EXCEPT ALL";
		break;
	case SET_INTERSECT:
		return "INTERSECT";
		break;
	case SET_INTERSECT_ALL:
		return "INTERSECT ALL";
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
	}
	return "";
}

char *get_type_string(SqlValueType type) {
	switch(type) {
	case NUMERIC_LITERAL:
		return "NUMERIC";
	case INTEGER_LITERAL:
		return "INTEGER";
	case STRING_LITERAL:
		return "STRING";
	case BOOLEAN_VALUE:
		return "BOOLEAN";
	case PARAMETER_VALUE:
		return "PARAMETER";
	case COLUMN_REFERENCE:
	case CALCULATED_VALUE:
	case UNKNOWN_SqlValueType:
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
	}
	return "";
}

// Helper function that is invoked when we have to traverse a "column_list_alias_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
int populate_data_type_column_list_alias(SqlStatement *v, SqlValueType *type, boolean_t do_loop) {
	SqlColumnListAlias	*column_list_alias, *cur_column_list_alias;
	SqlValueType		child_type1;
	int			result = 0;

	*type = UNKNOWN_SqlValueType;
	if ((NULL != v) && (NULL != v->v.select))
	{
		// SqlColumnListAlias
		UNPACK_SQL_STATEMENT(column_list_alias, v, column_list_alias);
		cur_column_list_alias = column_list_alias;
		do {
			child_type1 = UNKNOWN_SqlValueType;
			// SqlColumnList
			result |= populate_data_type(cur_column_list_alias->column_list, &child_type1);
			if (UNKNOWN_SqlValueType == cur_column_list_alias->type) {
				cur_column_list_alias->type = child_type1;
			} else if (cur_column_list_alias->type != child_type1) {
				// This is currently possible if a column is explicitly typecast
				// Disable the below code until #304 is fixed at which point it is possible
				//	this code can be re-enabled.
				// assert(FALSE);
				// result |= 1;
			}
			*type = child_type1;
			cur_column_list_alias = cur_column_list_alias->next;
		} while (do_loop && (cur_column_list_alias != column_list_alias));
	}
	return result;
}

// Helper function that is invoked when we have to traverse a "column_list_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
int populate_data_type_column_list(SqlStatement *v, SqlValueType *type, boolean_t do_loop) {
	SqlColumnList	*column_list, *cur_column_list;
	int		result = 0;

	*type = UNKNOWN_SqlValueType;
	if ((NULL != v) && (NULL != v->v.select))
	{
		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		do {
			// SqlValue or SqlColumnAlias
			result |= populate_data_type(cur_column_list->value, type);
			cur_column_list = cur_column_list->next;
		} while (do_loop && (cur_column_list != column_list));
	}
	return result;
}

int populate_data_type(SqlStatement *v, SqlValueType *type) {
	SqlBinaryOperation	*binary = NULL;
	SqlCaseBranchStatement	*cas_branch, *cur_branch;
	SqlCaseStatement	*cas;
	SqlColumn		*column = NULL;
	SqlFunctionCall		*function_call;
	SqlJoin			*start_join, *cur_join;
	SqlSetOperation		*set_operation;
	SqlTableAlias		*table_alias;
	SqlUnaryOperation	*unary = NULL;
	SqlValue		*value = NULL;
	SqlValueType		child_type1, child_type2;
	YYLTYPE			location;
	SqlSelectStatement	*select;
	int			result = 0;

	*type = UNKNOWN_SqlValueType;
	if (v == NULL || v->v.select == NULL)
		return 0;
	// Note: The below switch statement and the flow mirrors that in hash_canonical_query.c.
	//       Any change here or there needs to also be done in the other module.
	switch(v->type) {
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, v, cas);
		// We expect type to get overriden here; only the last type matters
		result |= populate_data_type(cas->value, type);
		result |= populate_data_type(cas->branches, type);
		result |= populate_data_type(cas->optional_else, type);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, v, cas_branch);
		cur_branch = cas_branch;
		do {
			result |= populate_data_type(cur_branch->condition, type);
			// TODO: we should check type here to make sure all branches have the same type
			result |= populate_data_type(cur_branch->value, type);
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, v, select);
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->select_list, &child_type1, TRUE);
		*type = child_type1;
		// SqlJoin
		result |= populate_data_type(select->table_list, &child_type1);
		// SqlValue (?)
		result |= populate_data_type(select->where_expression, &child_type1);
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->order_expression, &child_type1, TRUE);
		break;
	case function_call_STATEMENT:
		// TODO: we will need to know the return types of functions to do this
		// For now, just say STRING_LITERAL
		UNPACK_SQL_STATEMENT(function_call, v, function_call);
		// SqlColumnList
		result |= populate_data_type_column_list(function_call->parameters, type, TRUE);
		*type = STRING_LITERAL;
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
			result |= populate_data_type(value->v.calculated, &child_type1);
			*type = child_type1;
			break;
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case FUNCTION_NAME:
		case BOOLEAN_VALUE:
		case PARAMETER_VALUE:	   // Note: This is a possibility in "populate_data_type" but not in "hash_canonical_query"
		case UNKNOWN_SqlValueType: // Note: This is a possibility in "populate_data_type" but not in "hash_canonical_query"
			*type = value->type;
			break;
		case COLUMN_REFERENCE:
			/* If this happens it probably means it wasn't an extended reference
			 * which is not something we want to happen, the parser should expand
			 * all column references to be fully qualified
			 */
			assert(FALSE);
			result = 1;
			break;
		case NUL_VALUE:
			*type = NUL_VALUE;
			break;
		case COERCE_TYPE:
			*type = value->coerced_type;
			result |= populate_data_type(value->v.coerce_target, &child_type1);
			*v = *value->v.coerce_target;
			if (value_STATEMENT == v->type) {
				/* This is a work around the current way COERCE works
				 * It can only currently change the types of literals
				 * It does not work on columns or functions see issue #304
				 */
				if ((STRING_LITERAL == v->v.value->type) || (NUMERIC_LITERAL == v->v.value->type)
										|| (INTEGER_LITERAL == v->v.value->type))
					v->v.value->type = *type;
			}
			break;
		default:
			assert(FALSE);
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
		break;
	case column_alias_STATEMENT:
		if (column_list_alias_STATEMENT == v->v.column_alias->column->type) {
			result |= populate_data_type(v->v.column_alias->column, type);
		} else {
			UNPACK_SQL_STATEMENT(column, v->v.column_alias->column, column);
			switch(column->type) {
			case CHARACTER_STRING_TYPE:
				*type = STRING_LITERAL;
				break;
			case NUMERIC_TYPE:
				*type = NUMERIC_LITERAL;
				break;
			case INTEGER_TYPE:
				*type = INTEGER_LITERAL;
				break;
			case UNKNOWN_SqlDataType:
				// This could be a column that came in from a sub-query before when the sub-query column
				// was qualified (in "qualify_statement.c"). But by now we would have finished qualifying
				// all columns so we should be able to find out the column type at this point. Fix it now.
				assert(NULL != column->pre_qualified_cla);
				result |= populate_data_type(column->pre_qualified_cla->column_list, type);
				switch(*type) {
				case NUMERIC_LITERAL:
					column->type = NUMERIC_TYPE;
					break;
				case BOOLEAN_VALUE:
				case INTEGER_LITERAL:
					column->type = INTEGER_TYPE;
					break;
				case STRING_LITERAL:
					column->type = CHARACTER_STRING_TYPE;
					break;
				case NUL_VALUE:
					/* NULL values need to be treated as some known type. We choose CHARACTER_STRING_TYPE
					 * as this corresponds to the TEXT type of postgres to be compatible with it.
					 * See https://doxygen.postgresql.org/parse__coerce_8c.html#l01373 for more details.
					 */
					column->type = CHARACTER_STRING_TYPE;
					break;
				default:
					assert(FALSE);
					break;
				}
				break;
			default:
				assert(FALSE);
				ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
				result = 1;
				break;
			}
		}
		break;
	case column_list_STATEMENT:
		// Note: We do not loop through the list (just like is done in "hash_canonical_query")
		result |= populate_data_type_column_list(v, type, FALSE);
		break;
	case column_list_alias_STATEMENT:
		// Note: We do not loop through the list (just like is done in "hash_canonical_query")
		result |= populate_data_type_column_list_alias(v, type, FALSE);
		break;
	case table_STATEMENT:
		// Do nothing; we got here through a table_alias
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, v, table_alias);
		result |= populate_data_type(table_alias->table, type);
		assert((select_STATEMENT != table_alias->table->type)
			|| (table_alias->table->v.select->select_list == table_alias->column_list));
		if (select_STATEMENT != table_alias->table->type)
			result |= populate_data_type(table_alias->column_list, &child_type1);
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, v, binary);
		result |= populate_data_type(binary->operands[0], &child_type1);
		if (((BOOLEAN_IN == binary->operation) || (BOOLEAN_NOT_IN == binary->operation))
			&& (column_list_STATEMENT == binary->operands[1]->type))
		{	// SqlColumnList
			result |= populate_data_type_column_list(binary->operands[1], &child_type2, TRUE);
		} else {
			// SqlStatement (?)
			result |= populate_data_type(binary->operands[1], &child_type2);
		}
		if ((PARAMETER_VALUE == child_type1) || (NUL_VALUE == child_type1)) {
			child_type1 = child_type2;
		} else if ((PARAMETER_VALUE == child_type2) || (NUL_VALUE == child_type2)) {
			child_type2 = child_type1;
		} else if ((INTEGER_LITERAL == child_type1) && (NUMERIC_LITERAL == child_type2)){
			child_type1 = child_type2;
		} else if ((INTEGER_LITERAL == child_type2) && (NUMERIC_LITERAL == child_type1)){
			child_type2 = child_type1;
		}
		switch(binary->operation) {
		case ADDITION:
		case SUBTRACTION:
		case DIVISION:
		case MULTIPLICATION:
		case MODULO:
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
		if (child_type1 != child_type2) {
			int	i;

			ERROR(ERR_TYPE_MISMATCH, get_type_string(child_type1), get_type_string(child_type2));
			for (i = 0; i < 2; i++) {
				location = binary->operands[i]->loc;
				yyerror(NULL, NULL, &binary->operands[i], NULL, NULL);
			}
			result = 1;
		}
		break;
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(set_operation, v, set_operation);
		result |= populate_data_type(set_operation->operand[0], &child_type1);
		*type = child_type1;
		result |= populate_data_type(set_operation->operand[1], &child_type2);
		/* Now that the types of operands to the SET operation have been populated, do some more checks of
		 * whether the # and types of columns on both operands match. If not issue error.
		 */
		{
			SqlTableAlias		*table_alias[2];
			SqlColumnListAlias	*cur_cla[2], *start_cla[2];
			SqlStatement		*sql_stmt;
			boolean_t		terminate_loop[2] = {FALSE, FALSE};
			SqlColumnListAlias	*type_mismatch_cla[2] = {NULL, NULL};
			int			i;

			for (i = 0; i < 2; i++)
			{
				sql_stmt = drill_to_table_alias(set_operation->operand[i]);
				UNPACK_SQL_STATEMENT(table_alias[i], sql_stmt, table_alias);
				assert(NULL != table_alias[i]->column_list);
				UNPACK_SQL_STATEMENT(start_cla[i], table_alias[i]->column_list, column_list_alias);
				cur_cla[i] = start_cla[i];
			}
			do {
				/* Note: NULL should match against any non-NULL type hence the NUL_VALUE check below */
				if ((NULL == type_mismatch_cla[0]) && (cur_cla[0]->type != cur_cla[1]->type)
					&& (NUL_VALUE != cur_cla[0]->type) && (NUL_VALUE != cur_cla[1]->type))
				{
					type_mismatch_cla[0] = cur_cla[0];
					type_mismatch_cla[1] = cur_cla[1];
				}
				for (i = 0; i < 2; i++)
				{
					cur_cla[i] = cur_cla[i]->next;
					if (cur_cla[i] == start_cla[i])
						terminate_loop[i] = TRUE;
				}
			} while (!terminate_loop[0] && !terminate_loop[1]);
			if (terminate_loop[0] != terminate_loop[1]) {
				// The # of columns in the two operands (of the SET operation) do not match. Issue error.
				ERROR(ERR_SETOPER_NUMCOLS_MISMATCH, get_setoperation_string(set_operation->type));
				location = ((!terminate_loop[0]) ? cur_cla[0]->column_list->loc : cur_cla[1]->column_list->loc);
				yyerror(&location, NULL, NULL, NULL, NULL);
				result = 1;
			} else if (NULL != type_mismatch_cla[0]) {
				// The type of one column in the two operands (of the SET operation) do not match. Issue error.
				ERROR(ERR_SETOPER_TYPE_MISMATCH, get_setoperation_string(set_operation->type),		\
					get_type_string(type_mismatch_cla[0]->type), get_type_string(type_mismatch_cla[1]->type));
				location = type_mismatch_cla[0]->column_list->loc;
				yyerror(&location, NULL, NULL, NULL, NULL);
				location = type_mismatch_cla[1]->column_list->loc;
				yyerror(&location, NULL, NULL, NULL, NULL);
				result = 1;
			}
		}
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, v, unary);
		result |= populate_data_type(unary->operand, &child_type1);
		*type = child_type1;
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		result = 1;
	}
	return result;
}
