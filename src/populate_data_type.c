/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_type_check.h"

// Compare TYPE1 and TYPE2 and throw ERR_TYPE if not equal
#define CHECK_TYPE_AND_BREAK_ON_MISMATCH(TYPE1, TYPE2, ERR_TYPE, CUR_BRANCH_VALUE, NEXT_BRANCH_VALUE, RESULT)            \
	{                                                                                                                \
		if ((TYPE1) != (TYPE2)) {                                                                                \
			ERROR((ERR_TYPE), get_user_visible_type_string((TYPE1)), get_user_visible_type_string((TYPE2))); \
			yyerror(NULL, NULL, (CUR_BRANCH_VALUE), NULL, NULL, NULL);                                       \
			if (NULL != (NEXT_BRANCH_VALUE))                                                                 \
				yyerror(NULL, NULL, (NEXT_BRANCH_VALUE), NULL, NULL, NULL);                              \
			RESULT = 1;                                                                                      \
			break;                                                                                           \
		}                                                                                                        \
	}

// Helper function that is invoked when we have to traverse a "column_list_alias_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
int populate_data_type_column_list_alias(SqlStatement *v, SqlValueType *type, boolean_t do_loop, ParseContext *parse_context) {
	SqlColumnListAlias *column_list_alias, *cur_column_list_alias;
	SqlValueType	    child_type;
	int		    result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		// SqlColumnListAlias
		UNPACK_SQL_STATEMENT(column_list_alias, v, column_list_alias);
		cur_column_list_alias = column_list_alias;
		do {
			child_type = UNKNOWN_SqlValueType;
			// SqlColumnList
			result |= populate_data_type(cur_column_list_alias->column_list, &child_type, parse_context);
			if (UNKNOWN_SqlValueType == cur_column_list_alias->type) {
				cur_column_list_alias->type = child_type;
			} else if (cur_column_list_alias->type != child_type) {
				// This is currently possible if a column is explicitly typecast
				// Disable the below code until #304 is fixed at which point it is possible
				//	this code can be re-enabled.
				// assert(FALSE);
				// result |= 1;
			}
			*type = child_type;
			cur_column_list_alias = cur_column_list_alias->next;
		} while (do_loop && (cur_column_list_alias != column_list_alias));
	}
	return result;
}

// Helper function that is invoked when we have to traverse a "column_list_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
// If `do_loop` is set and `callback` is not NULL, it will be called for each type in the list.
int populate_data_type_column_list(SqlStatement *v, SqlValueType *type, boolean_t do_loop, DataTypeCallback callback,
				   ParseContext *parse_context) {
	SqlColumnList *column_list, *cur_column_list;
	SqlValueType   current_type;
	int	       result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		SqlStatement *first_value;

		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		first_value = NULL; /* needed to appease static code checkers from false warnings */
		do {
			// SqlValue or SqlColumnAlias
			current_type = UNKNOWN_SqlValueType;
			result |= populate_data_type(cur_column_list->value, &current_type, parse_context);
			if (result) {
				break;
			}
			if (UNKNOWN_SqlValueType != *type) {
				if (NULL != callback) {
					result |= callback(type, &current_type, first_value, cur_column_list->value, parse_context);
					if (result) {
						break;
					}
				}
			} else {
				first_value = cur_column_list->value;
			}
			cur_column_list = cur_column_list->next;
			*type = current_type;
		} while (do_loop && (cur_column_list != column_list));
	}
	return result;
}

int populate_data_type(SqlStatement *v, SqlValueType *type, ParseContext *parse_context) {
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlCaseStatement *	cas;
	SqlColumn *		column, *start_column;
	SqlCoalesceCall *	coalesce_call;
	SqlArray *		array;
	SqlGreatest *		greatest_call;
	SqlLeast *		least_call;
	SqlNullIf *		null_if;
	SqlAggregateFunction *	aggregate_function;
	SqlSetOperation *	set_operation;
	SqlTableAlias *		table_alias;
	SqlValue *		value;
	SqlValueType		child_type[2];
	SqlSelectStatement *	select;
	int			result;
	SqlTableValue *		table_value;
	SqlRowValue *		row_value, *start_row_value;
	int			num_columns;
	int			colno;
	SqlInsertStatement *	insert;

	assert(NULL != parse_context); /* helps distinguish "populate_data_type.c" from "qualify_check_constraint.c" when
					* calling common functions like "function_call_data_type_check()".
					*/
	result = 0;
	if (v == NULL || v->v.select == NULL)
		return 0;
	// Note: The below switch statement and the flow mirrors that in hash_canonical_query.c.
	//       Any change here or there needs to also be done in the other module.
	switch (v->type) {
	case cas_STATEMENT:
		*type = UNKNOWN_SqlValueType;
		UNPACK_SQL_STATEMENT(cas, v, cas);
		// We expect type to get overriden here; only the last type matters
		result |= populate_data_type(cas->value, type, parse_context);
		if (result) {
			break;
		}
		// Pass CASE value type to the next call of populate_data_type to compare
		// CASE value and WHEN condition result type.
		child_type[0] = *type;
		result |= populate_data_type(cas->branches, &child_type[0], parse_context);
		if (result) {
			break;
		}
		if (NULL != cas->optional_else) { // No need to validate types if ELSE not present
			result |= populate_data_type(cas->optional_else, &child_type[1], parse_context);
			if (result) {
				break;
			}
			// SQL NULL values are acceptable in CASE branches so CAST them appropriately
			CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result, parse_context);
			if (result) {
				break;
			}
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type[0], child_type[1], ERR_CASE_BRANCH_TYPE_MISMATCH,
							 &cas->branches->v.cas_branch->value, &cas->optional_else, result);
			if (result) {
				break;
			}
		}
		*type = child_type[0];
		break;
	case cas_branch_STATEMENT:
		// CASE value type is stored in *type
		// UNKNOWN_SqlValueType is a possible type for CASE value as it is optional in case_STATEMENTS.
		// In such a case consider it to be equal to BOOLEAN_VALUE.
		if (UNKNOWN_SqlValueType == *type)
			*type = BOOLEAN_VALUE;
		UNPACK_SQL_STATEMENT(cas_branch, v, cas_branch);
		cur_branch = cas_branch;
		result |= populate_data_type(cur_branch->value, &child_type[0], parse_context);
		if (result) {
			break;
		}
		do {
			result |= populate_data_type(cur_branch->condition, &child_type[1], parse_context);
			if (result) {
				break;
			}
			assert(UNKNOWN_SqlValueType != child_type[1]);
			// SQL NULL values are acceptable for CASE value and WHEN condition type so CAST them appropriately
			CAST_AMBIGUOUS_TYPES(*type, child_type[1], result, parse_context);
			if (result) {
				break;
			}
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type[1], *type, ERR_CASE_VALUE_TYPE_MISMATCH, &cur_branch->condition,
							 NULL, result);
			if (result) {
				break;
			}
			if (cas_branch != cur_branch->next) {
				result |= populate_data_type(cur_branch->next->value, &child_type[1], parse_context);
				if (result) {
					break;
				}
				// SQL NULL values are acceptable in CASE branches so CAST them appropriately
				CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result, parse_context);
				if (result) {
					break;
				}
				CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type[0], child_type[1], ERR_CASE_BRANCH_TYPE_MISMATCH,
								 &cur_branch->value, &cur_branch->next->value, result);
				if (result) {
					break;
				}
				child_type[0] = child_type[1];
			}
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		if (result) {
			break;
		}
		*type = child_type[0];
		break;
	case insert_STATEMENT:
		UNPACK_SQL_STATEMENT(insert, v, insert);
		result |= populate_data_type(insert->src_table_alias_stmt, &child_type[0], parse_context);
		if (result) {
			break;
		}
		result |= check_column_lists_for_type_match(v);
		break;
	case delete_from_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlDeleteFromStatement *delete;

		UNPACK_SQL_STATEMENT(delete, v, delete_from);
		result |= populate_data_type(delete->src_join, type, parse_context);
		if (result) {
			break;
		}
		if (NULL != delete->where_clause) {
			result |= populate_data_type(delete->where_clause, &child_type[0], parse_context);
			if (result) {
				break;
			}
			if (!IS_BOOLEAN_TYPE(child_type[0])) {
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[0], "boolean operations", &delete->where_clause, result);
				if (result) {
					break;
				}
			}
		}
		break;
	case update_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlUpdateStatement *update;

		UNPACK_SQL_STATEMENT(update, v, update);
		result |= populate_data_type(update->src_join, type, parse_context);
		if (result) {
			break;
		}
		if (NULL != update->where_clause) {
			result |= populate_data_type(update->where_clause, &child_type[0], parse_context);
			if (result) {
				break;
			}
			if (!IS_BOOLEAN_TYPE(child_type[0])) {
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[0], "boolean operations", &update->where_clause, result);
				if (result) {
					break;
				}
			}
		}

		SqlUpdateColumnValue *ucv, *ucv_head;
		ucv_head = update->col_value_list;
		ucv = ucv_head;
		do {
			UNPACK_SQL_STATEMENT(column, ucv->col_name, column);
			child_type[0] = get_sqlvaluetype_from_sqldatatype(column->data_type_struct.data_type, FALSE);
			result |= populate_data_type(ucv->col_value, &child_type[1], parse_context);
			if (result) {
				break;
			}
			CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result, parse_context);
			if (result) {
				break;
			}
			if (child_type[0] != child_type[1]) {
				ERROR(ERR_TYPE_MISMATCH, get_user_visible_type_string(child_type[0]),
				      get_user_visible_type_string(child_type[1]));
				yyerror(&ucv->col_name->loc, NULL, NULL, NULL, NULL, NULL);
				yyerror(&ucv->col_value->loc, NULL, NULL, NULL, NULL, NULL);
				result = 1;
				break;
			}
			ucv = ucv->next;
		} while (ucv != ucv_head);
		if (result) {
			break;
		}
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, v, select);
		// SqlJoin
		result |= populate_data_type(select->table_list, &child_type[0], parse_context);
		if (result) {
			break;
		}
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->select_list, &child_type[0], TRUE, parse_context);
		if (result) {
			break;
		}
		*type = child_type[0];
		// SqlValue (?)
		if (NULL != select->where_expression) {
			result |= populate_data_type(select->where_expression, &child_type[0], parse_context);
			if (result) {
				break;
			}
			if (!IS_BOOLEAN_TYPE(child_type[0])) {
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[0], "boolean operations", &select->where_expression,
							       result);
				if (result) {
					break;
				}
			}
		}
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->group_by_expression, &child_type[0], TRUE, parse_context);
		if (result) {
			break;
		}
		// SqlValue (?)
		if (NULL != select->having_expression) {
			result |= populate_data_type(select->having_expression, &child_type[0], parse_context);
			if (result) {
				break;
			}
			if (!IS_BOOLEAN_TYPE(child_type[0])) {
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[0], "boolean operations", &select->having_expression,
							       result);
				if (result) {
					break;
				}
			}
		}
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->order_by_expression, &child_type[0], TRUE, parse_context);
		if (result) {
			break;
		}
		/* While we are anyways descending down the query and subqueries (if any), take this opportunity to do some
		 * parse tree optimization if possible. Optimize subqueries first before doing the parent query. Hence the
		 * placement of this at the very end of the "case" block.
		 */
		parse_tree_optimize(select);
		break;
	case function_call_STATEMENT:;
		SqlFunctionCall *function_call;
		UNPACK_SQL_STATEMENT(function_call, v, function_call);
		result = function_call_data_type_check(function_call, type, parse_context, NULL);
		break;
	case array_STATEMENT:
		UNPACK_SQL_STATEMENT(array, v, array);
		result |= populate_data_type(array->argument, type, parse_context);
		/* Currently only ARRAY(single_column_subquery) is supported. In this case, "array->argument" points to a
		 * table_alias_STATEMENT type structure. And so any errors will show up in the above call. No additional
		 * errors possible in the "array" structure. This might change once more features of ARRAY() are supported
		 * at which point the code here might need more error checking.
		 */
		break;
	case coalesce_STATEMENT:
		UNPACK_SQL_STATEMENT(coalesce_call, v, coalesce);
		// SqlColumnList
		result |= populate_data_type_column_list(coalesce_call->arguments, type, TRUE, ensure_same_type, parse_context);
		// NOTE: if the types of the parameters do not match, no error is issued.
		// This matches the behavior of sqlite but not that of Postgres and Oracle DB.
		break;
	case greatest_STATEMENT:
		UNPACK_SQL_STATEMENT(greatest_call, v, greatest);
		// SqlColumnList
		result |= populate_data_type_column_list(greatest_call->arguments, type, TRUE, ensure_same_type, parse_context);
		break;
	case least_STATEMENT:
		UNPACK_SQL_STATEMENT(least_call, v, least);
		// SqlColumnList
		result |= populate_data_type_column_list(least_call->arguments, type, TRUE, ensure_same_type, parse_context);
		break;
	case null_if_STATEMENT:;
		SqlValueType tmp;

		UNPACK_SQL_STATEMENT(null_if, v, null_if);
		// SqlColumnList
		result |= populate_data_type(null_if->left, type, parse_context);
		if (result) {
			break;
		}
		result |= populate_data_type(null_if->right, &tmp, parse_context);
		if (result) {
			break;
		}
		result |= ensure_same_type(type, &tmp, null_if->left, null_if->right, parse_context);
		break;
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, v, aggregate_function);
		// SqlColumnList : table.* usage will have more than one node so loop through
		result |= populate_data_type_column_list(aggregate_function->parameter, type, TRUE, NULL, parse_context);
		if (result) {
			break;
		}
		// Note that COUNT(...) is always an INTEGER type even though ... might be a string type column.
		switch (aggregate_function->type) {
		case AGGREGATE_COUNT_DISTINCT:
			assert(TABLE_ASTERISK != (*type));
			/* The above assert is valid as count(DISTINCT table.*) value would have been expanded at
			 * qualify_statement() aggregate_function_STATEMENT case to column_list of column_alias values
			 * by "process_table_asterisk_cl()" call. And this is why we are also guaranteed that "*type"
			 * would be initialized in the "populate_data_type_column_list()" call above as there is at least
			 * one column in the list that would have been processed and "*type" would be initialized to the
			 * type of the last column in that list. For example, in the "AGGREGATE_COUNT_ASTERISK" case below,
			 * we are not guaranteed "*type" is initialized and hence cannot have a similar assert.
			 */
			*type = INTEGER_LITERAL;
			break;
		case AGGREGATE_COUNT_ASTERISK:
		case AGGREGATE_COUNT:
			*type = INTEGER_LITERAL;
			break;
		case AGGREGATE_AVG:
		case AGGREGATE_AVG_DISTINCT:
		case AGGREGATE_SUM:
		case AGGREGATE_SUM_DISTINCT:
			if ((TABLE_ASTERISK == *type) || (STRING_LITERAL == *type) || (BOOLEAN_VALUE == *type)) {
				/* TABLE_ASTERISK or STRING or BOOLEAN type cannot be input for the AVG or SUM function so signal
				 * an error in that case.
				 */
				if (TABLE_ASTERISK == *type) {
					ERROR(ERR_MISTYPED_FUNCTION_TABLE_ASTERISK,
					      get_aggregate_func_name(aggregate_function->type),
					      get_user_visible_type_string(*type));
				} else {
					ERROR(ERR_MISTYPED_FUNCTION, get_aggregate_func_name(aggregate_function->type),
					      get_user_visible_type_string(*type));
				}
				yyerror(NULL, NULL, &aggregate_function->parameter, NULL, NULL, NULL);
				result = 1;
			}
			break;
		case AGGREGATE_MIN:
		case AGGREGATE_MAX:
			if ((TABLE_ASTERISK == *type) || (BOOLEAN_VALUE == *type)) {
				/* TABLE_ASTERISK or BOOLEAN type cannot be input for the MIN or MAX function so signal an error in
				 * that case. */
				ERROR(ERR_MISTYPED_FUNCTION, get_aggregate_func_name(aggregate_function->type),
				      get_user_visible_type_string(*type));
				yyerror(NULL, NULL, &aggregate_function->parameter, NULL, NULL, NULL);
				result = 1;
				break;
			}
			/* In this case, *type should be the same as the type of "aggregate_function->parameter"
			 * which is already set above so copy that over for later use in physical plan.
			 */
			aggregate_function->param_type = *type;
			break;
		default:
			assert(FALSE);
			break;
		}
		break;
	case join_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlJoin *start_join, *cur_join;

		UNPACK_SQL_STATEMENT(start_join, v, join);
		cur_join = start_join;
		do {
			result |= populate_data_type(cur_join->value, type, parse_context);
			if (result) {
				break;
			}
			if (NULL != cur_join->condition) {
				result |= populate_data_type(cur_join->condition, type, parse_context);
				if (result) {
					break;
				}
				if (!IS_BOOLEAN_TYPE(*type)) {
					ISSUE_TYPE_COMPATIBILITY_ERROR(*type, "boolean operations", &cur_join->condition, result);
					if (result) {
						break;
					}
				}
			}
			cur_join = cur_join->next;
		} while (cur_join != start_join);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, v, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			result |= populate_data_type(value->v.calculated, &child_type[0], parse_context);
			if (result) {
				break;
			}
			*type = child_type[0];
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
			/* If lexer determined that the value is an integer literal, use that more specific INTEGER_LITERAL type
			 * instead of a more generic NUMERIC_LITERAL type.
			 */
			*type = value->type;
			break;
		case FUNCTION_NAME:
			/* No need to do any qualification or type checking in this case */
			break;
		case TABLE_ASTERISK:
			/* We should not reach this case as TABLE_ASTERISK can only occur in aggregate function count usage
			 * and aggregate function has TABLE_ASTERISK enclosed in COLUMN_ALIAS, and COLUMN_ALIAS case itself
			 * takes care of returning the type.
			 */
			assert(FALSE);
			result = 1;
			break;
		case PARAMETER_VALUE: { // Note: This is a possibility in "populate_data_type" but not in "hash_canonical_query"
			long tmp_long;
			int  param_num;

			/* Set the type to "PARAMETER_VALUE" by default. Check if this is an extended query and if query
			 * parameter types have been specified in the "Parse" message. If so use that type instead.
			 */
			*type = PARAMETER_VALUE;
			if (parse_context->is_extended_query) {
				tmp_long = strtol(value->v.string_literal + 1, NULL, 10); /* + 1 to skip '$' prefix in '$1' etc. */
				if ((LONG_MIN != tmp_long) && (LONG_MAX != tmp_long)) {
					param_num = (int)tmp_long;
					parse_context->cur_param_num = param_num;
					if ((0 < param_num) && (param_num <= parse_context->num_bind_parm_types)) {
						/* Note down parameter number corresponding to this "SqlValue" structure
						 * for later use inside MAP_TYPE_TO_PARAMETER_VALUE macro.
						 */
						*type = get_sqlvaluetype_from_psql_type(parse_context->types[param_num - 1]);
					}
				} else {
					ERROR(ERR_LIBCALL, "strtol");
					result = 1;
				}
			}
			break;
		}
		case COLUMN_REFERENCE:
			/* If this happens it probably means it wasn't an extended reference
			 * which is not something we want to happen, the parser should expand
			 * all column references to be fully qualified
			 */
			assert(FALSE);
			result = 1;
			break;
		case COERCE_TYPE:
			result |= populate_data_type(value->v.coerce_target, &child_type[0], parse_context);
			if (result) {
				break;
			}
			/* Note down type of target before coerce */
			value->pre_coerced_type = child_type[0];
			/* At this time (Jan 2020), we allow any type to be coerced to any other type at parser time.
			 * Errors in type conversion, if any, should show up at run-time based on the actual values.
			 * But since our run-time is M and we currently only allow INTEGER/NUMERIC/STRING types,
			 * M will allow for converting between either of these types without any errors which is
			 * different from Postgres (where `'Zero'::integer` will cause an error). We will deal with
			 * this if users complain about this incompatibility with Postgres.
			 */
			*type = get_sqlvaluetype_from_sqldatatype(value->coerced_type.data_type, FALSE);
			break;
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
			/* These usages should not be possible. Assert accordingly. */
			assert(FALSE);
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
		break;
	case column_alias_STATEMENT:
		if (is_stmt_table_asterisk(v->v.column_alias->column)) {
			/* When table.* is used with aggregate function other than COUNT
			 * we can come across this case. Retain type for aggregate_function_STATEMENT case
			 * to throw appropriate error.
			 */
			*type = TABLE_ASTERISK;
		} else if (column_list_alias_STATEMENT == v->v.column_alias->column->type) {
			result |= populate_data_type(v->v.column_alias->column, type, parse_context);
			if (result) {
				break;
			}
		} else {
			UNPACK_SQL_STATEMENT(column, v->v.column_alias->column, column);
			*type = get_sqlvaluetype_from_sqldatatype(column->data_type_struct.data_type, FALSE);
		}
		break;
	case column_list_STATEMENT:
		// Note: We do not loop through the list (just like is done in "hash_canonical_query")
		result |= populate_data_type_column_list(v, type, FALSE, NULL, parse_context);
		break;
	case column_list_alias_STATEMENT:
		// Note: We do not loop through the list (just like is done in "hash_canonical_query")
		result |= populate_data_type_column_list_alias(v, type, FALSE, parse_context);
		break;
	case create_table_STATEMENT:
		// Do nothing; we got here through a table_alias
		break;
	case table_value_STATEMENT:
		/* For a table constructed using the VALUES clause, go through each value specified and determine
		 * its type. Verify all rows have same type for each column.
		 */
		UNPACK_SQL_STATEMENT(table_value, v, table_value);
		UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
		start_row_value = row_value;
		num_columns = row_value->num_columns;
		assert(num_columns);

		SqlValueType * type_array;
		SqlStatement **first_value;
		type_array = (SqlValueType *)calloc(num_columns, sizeof(SqlValueType));
		first_value = (SqlStatement **)calloc(num_columns, sizeof(SqlStatement *));
		do {
			SqlColumnList *start_column_list, *cur_column_list;

			UNPACK_SQL_STATEMENT(start_column_list, row_value->value_list, column_list);
			cur_column_list = start_column_list;
			colno = 0;
			do {
				SqlValueType current_type;

				// SqlValue or SqlColumnAlias
				result |= populate_data_type(cur_column_list->value, &current_type, parse_context);
				if (result) {
					break;
				}
				if (start_row_value == row_value) {
					/* This is the first row. We don't have any type to compare against.
					 * Just record this type for now.
					 */
					assert(UNKNOWN_SqlValueType == type_array[colno]);
					assert(UNKNOWN_SqlValueType != current_type);
					type_array[colno] = current_type;
					first_value[colno] = cur_column_list->value;
				} else {
					/* Compare type determined for this row against noted down type from previous row */
					assert(UNKNOWN_SqlValueType != type_array[colno]);
					result |= ensure_same_type(&type_array[colno], &current_type, first_value[colno],
								   cur_column_list->value, parse_context);
					if (result) {
						break;
					}
				}
				colno++;
				cur_column_list = cur_column_list->next;
			} while ((cur_column_list != start_column_list));
			if (result) {
				break;
			}
			assert(colno == num_columns); /* every row should have same number of columns as first row */
			row_value = row_value->next;
		} while (row_value != start_row_value);
		/* Note: Cannot "break" if "result" is non-zero like in other places because we need to do some "free()" calls */
		if (!result) {
			/* Now that we have scanned all rows of the VALUES table, store the final determined column type.
			 * Note that if one row had a column type of NUL_VALUE and the next row had a type of NUMERIC_LITERAL,
			 * the final column type would end up being NUMERIC_LITERAL.
			 */
			start_column = table_value->column;
			column = start_column;
			colno = 0;
			do {
				assert(UNKNOWN_SqlDataType == column->data_type_struct.data_type);
				column->data_type_struct.data_type = get_sqldatatype_from_sqlvaluetype(type_array[colno]);
				column->data_type_struct.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;
				column->data_type_struct.scale = SCALE_UNSPECIFIED;
				column->data_type_struct.size_or_precision_parameter_index = 0;
				column->data_type_struct.scale_parameter_index = 0;
				column = column->next;
				colno++;
			} while (column != start_column);
			assert(colno == num_columns);
			*type = type_array[0]; /* Return the type of the first column in the VALUES clause */
		}
		free(type_array);
		free(first_value);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, v, table_alias);
		result |= populate_data_type(table_alias->table, type, parse_context);
		assert((select_STATEMENT != table_alias->table->type)
		       || (table_alias->table->v.select->select_list == table_alias->column_list));
		if (result) {
			break;
		}
		if (select_STATEMENT != table_alias->table->type) {
			/* Note: In case "table_alias->table" is of type "table_value_STATEMENT", the above call would have
			 * determined the type of the "SqlColumn" structures making up that table based on the actual values
			 * data supplied. That will then need to be propagated to the associated "SqlColumnListAlias" structures
			 * in the below call.
			 */
			result
			    |= populate_data_type_column_list_alias(table_alias->column_list, &child_type[0], TRUE, parse_context);
		}
		break;
	case binary_STATEMENT:;
		SqlBinaryOperation *binary;

		UNPACK_SQL_STATEMENT(binary, v, binary);
		result |= populate_data_type(binary->operands[0], &child_type[0], parse_context);
		if (result) {
			break;
		}
		if (((BOOLEAN_IN == binary->operation) || (BOOLEAN_NOT_IN == binary->operation))
		    && (column_list_STATEMENT == binary->operands[1]->type)) { // SqlColumnList
			result |= populate_data_type_column_list(binary->operands[1], &child_type[1], TRUE, NULL, parse_context);
		} else {
			// SqlStatement (?)
			result |= populate_data_type(binary->operands[1], &child_type[1], parse_context);
		}
		if (result) {
			break;
		}
		result = binary_operation_data_type_check(binary, child_type, type, parse_context);
		break;
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(set_operation, v, set_operation);
		result |= populate_data_type(set_operation->operand[0], &child_type[0], parse_context);
		if (result) {
			break;
		}
		*type = child_type[0];
		result |= populate_data_type(set_operation->operand[1], &child_type[1], parse_context);
		if (result) {
			break;
		}
		/* Now that the types of operands to the SET operation have been populated, do some more checks of
		 * whether the # and types of columns on both operands match. If not issue error.
		 */
		result |= check_column_lists_for_type_match(v);
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;

		UNPACK_SQL_STATEMENT(unary, v, unary);
		result |= populate_data_type(unary->operand, &child_type[0], parse_context);
		if (result) {
			break;
		}
		result = unary_operation_data_type_check(unary, child_type, type);
		break;
	case create_function_STATEMENT:
	case drop_table_STATEMENT:
	case drop_function_STATEMENT:
	case column_STATEMENT:
	case parameter_type_list_STATEMENT:
	case constraint_STATEMENT:
	case keyword_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
	case set_STATEMENT:
	case show_STATEMENT:
	case no_data_STATEMENT:
	case delim_char_list_STATEMENT:
	case index_STATEMENT:
	case data_type_struct_STATEMENT:
	case join_type_STATEMENT:
	case discard_all_STATEMENT:
	case row_value_STATEMENT:
	case history_STATEMENT:
	case invalid_STATEMENT:
		/* Do not add "default:" case as we want to enumerate each explicit case here instead of having a
		 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
		 */
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		result = 1;
		break;
	}
	return result;
}
