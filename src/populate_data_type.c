/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

#define ISSUE_ERR_AND_BREAK_IF_SUBQUERY_HAS_MULTIPLE_COLUMNS(V, PARENT_STMT, RESULT)                                            \
	{                                                                                                                       \
		/* Even column_alias_STATEMENT can reach this code in the case shown below                                      \
		 * select * from (select ((values(lastname))) from names);                                                      \
		 */                                                                                                             \
		assert((NULL == PARENT_STMT)                                                                                    \
		       || ((column_list_alias_STATEMENT != PARENT_STMT->type) && (column_list_STATEMENT != PARENT_STMT->type)   \
			   && (table_alias_STATEMENT != PARENT_STMT->type) && (set_operation_STATEMENT != PARENT_STMT->type))); \
                                                                                                                                \
		boolean_t do_num_cols_check;                                                                                    \
                                                                                                                                \
		do_num_cols_check                                                                                               \
		    = ((NULL != PARENT_STMT)                                                                                    \
		       && ((unary_STATEMENT != PARENT_STMT->type) || (BOOLEAN_EXISTS != PARENT_STMT->v.unary->operation)));     \
		if (do_num_cols_check) {                                                                                        \
			assert((table_alias_STATEMENT == V->type) || (set_operation_STATEMENT == V->type));                     \
                                                                                                                                \
			SqlStatement *table_alias_stmt;                                                                         \
			table_alias_stmt = drill_to_table_alias(V);                                                             \
                                                                                                                                \
			SqlTableAlias *table_alias;                                                                             \
			UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);                                       \
			int num_of_columns = get_num_cols_in_table_alias(table_alias);                                          \
			if (1 < num_of_columns) {                                                                               \
				ERROR(ERR_SUBQUERY_ONE_COLUMN, "");                                                             \
				yyerror(NULL, NULL, &V, NULL, NULL, NULL);                                                      \
				RESULT = 1;                                                                                     \
			}                                                                                                       \
			if (RESULT) {                                                                                           \
				break;                                                                                          \
			}                                                                                                       \
		}                                                                                                               \
	}

// Helper function that is invoked when we have to traverse a "column_list_alias_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
int populate_data_type_column_list_alias(SqlStatement *v, SqlValueType *type, SqlStatement *parent_stmt, boolean_t do_loop,
					 ParseContext *parse_context, SqlColumnListAlias *fix_type_cla) {
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
			SqlValueType *fix_type2, tmp_type;

			child_type = UNKNOWN_SqlValueType;
			// SqlColumnList
			if (NULL != fix_type_cla) {
				assert(UNKNOWN_SqlValueType != cur_column_list_alias->type); /* should have been initialized
											      * in prior call to this function.
											      */
				tmp_type = fix_type_cla->type;
				fix_type2 = &tmp_type;
				fix_type_cla = fix_type_cla->next;
			} else {
				fix_type2 = NULL;
			}
			result |= populate_data_type(cur_column_list_alias->column_list, &child_type, parent_stmt, parse_context,
						     fix_type2);
			assert((UNKNOWN_SqlValueType == cur_column_list_alias->type)
			       || (BOOLEAN_OR_STRING_LITERAL == cur_column_list_alias->type)
			       || (cur_column_list_alias->type == child_type));
			cur_column_list_alias->type = child_type;
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
int populate_data_type_column_list(SqlStatement *v, SqlValueType *type, SqlStatement *parent_stmt, boolean_t do_loop,
				   DataTypeCallback callback, ParseContext *parse_context, SqlValueType *fix_type) {
	SqlColumnList *column_list, *cur_column_list;
	SqlValueType   current_type;
	int	       result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		SqlStatement *first_value;
		boolean_t     saw_boolean_or_string_literal;

		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		saw_boolean_or_string_literal = FALSE;
		first_value = NULL; /* needed to appease static code checkers from false warnings */
		do {
			// SqlValue or SqlColumnAlias
			current_type = UNKNOWN_SqlValueType;
			result |= populate_data_type(cur_column_list->value, &current_type, parent_stmt, parse_context, fix_type);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == current_type) {
				saw_boolean_or_string_literal = TRUE;
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
		if (!result && do_loop && saw_boolean_or_string_literal && (BOOLEAN_OR_STRING_LITERAL != current_type)) {
			/* Call "populate_data_type" again to reset BOOLEAN_OR_STRING_LITERAL occurrences to BOOLEAN_VALUE
			 * or STRING_LITERAL. Example query that needs this logic is
			 *	select true in (false, 't');
			 */
			SqlValueType fix_type2;

			assert(current_type == *type);
			assert((BOOLEAN_VALUE == current_type) || (STRING_LITERAL == current_type));
			fix_type2 = current_type;
			cur_column_list = column_list;
			do {
				result = populate_data_type(cur_column_list->value, &current_type, parent_stmt, parse_context,
							    &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				cur_column_list = cur_column_list->next;
			} while (do_loop && (cur_column_list != column_list));
		}
	}
	return result;
}

/*
 * `parent_stmt` is an input parameter which informs the current invocation what the holding `stmt` is in the parse tree. This
 * information is used by ISSUE_ERR_AND_BREAK_IF_SUBQUERY_HAS_MULTIPLE_COLUMNS() invocation in `table_alias_STATEMENT` and
 * `set_operation_STATEMENT` to determine if multiple columns are allowed or not for the current `stmt` being processed. Note that
 * the value of `parent_stmt` is not always the immediate parent, for example in case of `column_list_STATEMENT` and
 * `column_list_alias_STATEMENT` which represents a list, we are interested to know who holds the list to determine if multiple
 * columns are allowed or not. In such cases the `parent_stmt` will represent the grand-parent holding the list. Also, note that in
 * some cases like the insert_STATEMENT, join_STATEMENT etc, NULL is passed as argument of `parent_stmt`. This is because we want
 * to allow multiple columns in subqueries for such statement types, ISSUE_ERR_AND_BREAK_IF_SUBQUERY_HAS_MULTIPLE_COLUMNS sees that
 * the value of `parent_stmt` is NULL and in such case no additional processing is done.
 */
int populate_data_type(SqlStatement *v, SqlValueType *type, SqlStatement *parent_stmt, ParseContext *parse_context,
		       SqlValueType *fix_type) {
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlCaseStatement *	cas;
	SqlColumn *		column, *start_column;
	SqlCoalesceCall *	coalesce_call;
	SqlArray *		array;
	SqlGreatest *		greatest_call;
	SqlLeast *		least_call;
	SqlNullIf *		null_if;
	SqlSetOperation *	set_operation;
	SqlTableAlias *		table_alias;
	SqlValue *		value;
	SqlValueType		child_type[2];
	SqlSelectStatement *	select;
	int			result;
	SqlTableValue *		table_value;
	SqlRowValue *		row_value, *start_row_value;
	int			num_columns;
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
		UNPACK_SQL_STATEMENT(cas, v, cas);
		/* There are two valid ways of specifying case statements in SQL.
		 *
		 * The first takes a variable called case_value and matches it with some statement_list.
		 * 	CASE case_value
		 * 	    WHEN when_value THEN statement_list
		 * 	    [WHEN when_value THEN statement_list] ...
		 * 	    [ELSE statement_list]
		 * 	END
		 *
		 * The second considers a search_condition instead of variable equality and executes the
		 * statement_list accordingly.
		 * 	CASE
		 * 	    WHEN search_condition THEN statement_list
		 * 	    [WHEN search_condition THEN statement_list] ...
		 * 	    [ELSE statement_list]
		 * 	END
		 *
		 * cas->value would be NULL in the second way. Handle that accordingly.
		 */
		if (NULL != cas->value) {
			/* This is the first way of specifying case statements.
			 * That is, "case_value" is specified. Note down its type in "*type".
			 * This is later used as the expected type of the WHEN arguments that follow.
			 */
			result |= populate_data_type(cas->value, type, v, parse_context, NULL);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == *type) {
				SqlValueType fix_type2;

				fix_type2 = STRING_LITERAL;
				result |= populate_data_type(cas->value, type, v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(STRING_LITERAL == *type);
			}
		} else {
			/* This is the second way of specifying case statements.
			 * In this case, the "WHEN search_condition" is a boolean type expression.
			 * So note that down as the expected type of the WHEN argument.
			 */
			*type = BOOLEAN_VALUE;
		}
		assert(NULL != cas->branches);
		/* Compare "*type" (type of "case_value") against type of WHEN argument ("when_value") in the below call.
		 * Issue error if they don't match.
		 */
		result |= populate_data_type(cas->branches, type, v, parse_context, NULL);
		if (result) {
			break;
		}
		/* "*type" at this point stores the type of the THEN argument ("statement_list") */
		if (NULL != cas->optional_else) {
			/* ELSE is present. Check that type of ELSE argument matches with type of THEN arguments till now */
			result |= populate_data_type(cas->optional_else, &child_type[0], v, parse_context, NULL);
			if (result) {
				break;
			}
			// SQL NULL values are acceptable in CASE branches so CAST them appropriately
			CAST_AMBIGUOUS_TYPES(*type, child_type[0], result, parse_context);
			if (result) {
				break;
			}
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(*type, child_type[0], ERR_CASE_BRANCH_TYPE_MISMATCH,
							 &cas->branches->v.cas_branch->value, &cas->optional_else, result);
			if (result) {
				break;
			}
		}
		/* Now that we know types of all CASE code paths match, check if that type is a table.asterisk.
		 * If so issue error as that is not a valid type for CASE currently.
		 */
		if (TABLE_ASTERISK == *type) {
			ISSUE_TYPE_COMPATIBILITY_ERROR(*type, "case operation", &cas->branches->v.cas_branch->value, result);
		}
		if (BOOLEAN_OR_STRING_LITERAL == *type) {
			SqlValueType fix_type2;

			FIX_TYPE_TO_STRING_LITERAL(*type);
			fix_type2 = STRING_LITERAL;
			UNPACK_SQL_STATEMENT(cas_branch, cas->branches, cas_branch);
			cur_branch = cas_branch;
			do {
				result |= populate_data_type(cur_branch->value, type, v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert((fix_type2 == *type) || IS_NUL_VALUE(*type));
				cur_branch = cur_branch->next;
			} while (cur_branch != cas_branch);
			if (NULL != cas->optional_else) {
				result |= populate_data_type(cas->optional_else, type, v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd
						    call */
				assert((fix_type2 == *type) || IS_NUL_VALUE(*type));
			}
		}
		break;
	case cas_branch_STATEMENT:
		assert(UNKNOWN_SqlValueType != *type); /* "*type" stores the expected type of "cur_branch->condition" */
		UNPACK_SQL_STATEMENT(cas_branch, v, cas_branch);
		cur_branch = cas_branch;
		result |= populate_data_type(cur_branch->value, &child_type[0], v, parse_context, NULL);
		if (result) {
			break;
		}
		do {
			result |= populate_data_type(cur_branch->condition, &child_type[1], v, parse_context, NULL);
			if (result) {
				break;
			}
			assert(UNKNOWN_SqlValueType != child_type[1]);
			if (BOOLEAN_OR_STRING_LITERAL == child_type[1]) {
				SqlValueType fix_type2;

				/* Check if expected type is BOOLEAN or STRING. If so, fix child_type[1] accordingly. */
				switch (*type) {
				case BOOLEAN_VALUE:
					fix_type2 = BOOLEAN_VALUE;
					break;
				case STRING_LITERAL:
					fix_type2 = STRING_LITERAL;
					break;
				default:
					fix_type2 = UNKNOWN_SqlValueType;
					break;
				}
				if (UNKNOWN_SqlValueType != fix_type2) {
					result |= populate_data_type(cur_branch->condition, &child_type[1], v, parse_context,
								     &fix_type2);
					assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd
							    call */
					UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
					assert(fix_type2 == child_type[1]);
				}
			}
			/* Compare expected ("*type") and actual type ("child_type[1]") of "cur_branch->condition" and issue
			 * error if they don't match.
			 */
			// SQL NULL values are acceptable for CASE value and WHEN condition type so CAST them appropriately
			CAST_AMBIGUOUS_TYPES(*type, child_type[1], result, parse_context);
			if (result) {
				break;
			}
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type[1], *type, ERR_CASE_VALUE_TYPE_MISMATCH, &cur_branch->condition,
							 NULL, result);
			assert(!result); /* because the above macro would have done a "break" otherwise */
			if (cas_branch != cur_branch->next) {
				result |= populate_data_type(cur_branch->next->value, &child_type[1], v, parse_context, NULL);
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
				assert(!result); /* because the above macro would have done a "break" otherwise */
				assert(child_type[0] == child_type[1]);
			}
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		if (result) {
			break;
		}
		/* Store type of the THEN argument in "*type" for caller to use to compare ELSE argument type if one exists */
		*type = child_type[0];
		break;
	case insert_STATEMENT:
		assert(NULL == fix_type); /* so no need to handle non-NULL fix_type scenario */
		UNPACK_SQL_STATEMENT(insert, v, insert);
		result |= populate_data_type(insert->src_table_alias_stmt, &child_type[0], NULL, parse_context, NULL);
		if (result) {
			break;
		}
		result |= check_column_lists_for_type_match(v, parse_context);
		break;
	case delete_from_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlDeleteFromStatement *delete;

		UNPACK_SQL_STATEMENT(delete, v, delete_from);
		result |= populate_data_type(delete->src_join, type, NULL, parse_context, NULL);
		if (result) {
			break;
		}
		if (NULL != delete->where_clause) {
			result |= populate_data_type(delete->where_clause, &child_type[0], NULL, parse_context, NULL);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == child_type[0]) {
				SqlValueType fix_type2;

				fix_type2 = BOOLEAN_VALUE;
				result |= populate_data_type(delete->where_clause, &child_type[0], NULL, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				assert(fix_type2 == child_type[0]);
				UNUSED(result); /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
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
		result |= populate_data_type(update->src_join, type, NULL, parse_context, NULL);
		if (result) {
			break;
		}
		if (NULL != update->where_clause) {
			result |= populate_data_type(update->where_clause, &child_type[0], NULL, parse_context, NULL);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == child_type[0]) {
				SqlValueType fix_type2;

				fix_type2 = BOOLEAN_VALUE;
				result |= populate_data_type(update->where_clause, &child_type[0], NULL, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				assert(fix_type2 == child_type[0]);
				UNUSED(result); /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
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
			boolean_t saw_boolean_or_string_literal;

			saw_boolean_or_string_literal = FALSE;
			UNPACK_SQL_STATEMENT(column, ucv->col_name, column);
			child_type[0] = get_sqlvaluetype_from_sqldatatype(column->data_type_struct.data_type, FALSE);
			if (keyword_STATEMENT == ucv->col_value->type) {
#ifndef NDEBUG
				SqlOptionalKeyword *keyword;
				UNPACK_SQL_STATEMENT(keyword, ucv->col_value, keyword);
				assert(OPTIONAL_DEFAULT == keyword->keyword);
				UNUSED(keyword);
#endif
				/* At present, DEFAULT value is allowed when UPDATE is being applied
				 * on an identity column. Identity columns are only of INTEGER type and
				 * since DEFAULT is specified set the value type to be INTEGER as well.
				 */
				assert(IS_COLUMN_IDENTITY(column));
				child_type[1] = INTEGER_LITERAL;
			} else {
				result |= populate_data_type(ucv->col_value, &child_type[1], v, parse_context, NULL);
				if (result) {
					break;
				}
				if (BOOLEAN_OR_STRING_LITERAL == child_type[1]) {
					saw_boolean_or_string_literal = TRUE;
					assert(BOOLEAN_OR_STRING_LITERAL != child_type[0]);
				}
				CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result, parse_context);
				if (result) {
					break;
				}
			}
			if (child_type[0] != child_type[1]) {
				ERROR(ERR_TYPE_MISMATCH, get_user_visible_type_string(child_type[0]),
				      get_user_visible_type_string(child_type[1]));
				yyerror(&ucv->col_name->loc, NULL, NULL, NULL, NULL, NULL);
				yyerror(&ucv->col_value->loc, NULL, NULL, NULL, NULL, NULL);
				result = 1;
				break;
			}
			if (saw_boolean_or_string_literal) {
				SqlValueType fix_type2;

				fix_type2 = child_type[1];
				assert((STRING_LITERAL == child_type[1]) || (BOOLEAN_VALUE == child_type[1]));
				result |= populate_data_type(ucv->col_value, &child_type[1], v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
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
		result |= populate_data_type(select->table_list, &child_type[0], NULL, parse_context, NULL);
		if (result) {
			break;
		}
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->select_list, &child_type[0], v, TRUE, parse_context, NULL);
		if (result) {
			break;
		}
		*type = child_type[0];
		// SqlValue (?)
		if (NULL != select->where_expression) {
			result |= populate_data_type(select->where_expression, &child_type[0], v, parse_context, NULL);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == child_type[0]) {
				SqlValueType fix_type2;

				fix_type2 = BOOLEAN_VALUE;
				result
				    |= populate_data_type(select->where_expression, &child_type[0], v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[0]);
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
		result |= populate_data_type_column_list_alias(select->group_by_expression, &child_type[0], v, TRUE, parse_context,
							       NULL);
		if (result) {
			break;
		}
		// SqlValue (?)
		if (NULL != select->having_expression) {
			result |= populate_data_type(select->having_expression, &child_type[0], v, parse_context, NULL);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == child_type[0]) {
				SqlValueType fix_type2;

				fix_type2 = BOOLEAN_VALUE;
				result
				    |= populate_data_type(select->having_expression, &child_type[0], v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[0]);
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
		result |= populate_data_type_column_list_alias(select->order_by_expression, &child_type[0], v, TRUE, parse_context,
							       NULL);
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
		result = function_call_data_type_check(v, type, parse_context, NULL);
		break;
	case array_STATEMENT:
		UNPACK_SQL_STATEMENT(array, v, array);
		result |= populate_data_type(array->argument, type, v, parse_context, NULL);
		/* Currently only ARRAY(single_column_subquery) is supported. In this case, "array->argument" points to a
		 * table_alias_STATEMENT type structure. And so any errors will show up in the above call. No additional
		 * errors possible in the "array" structure. This might change once more features of ARRAY() are supported
		 * at which point the code here might need more error checking.
		 */
		break;
	case coalesce_STATEMENT:
		UNPACK_SQL_STATEMENT(coalesce_call, v, coalesce);
		// SqlColumnList
		result |= populate_data_type_column_list(coalesce_call->arguments, type, v, TRUE, ensure_same_type, parse_context,
							 NULL);
		if ((!result) && (TABLE_ASTERISK == *type)) {
			assert(coalesce_call->arguments);
			ISSUE_TYPE_COMPATIBILITY_ERROR(*type, "coalesce operation", &coalesce_call->arguments, result);
		}
		break;
	case greatest_STATEMENT:
		UNPACK_SQL_STATEMENT(greatest_call, v, greatest);
		// SqlColumnList
		result |= populate_data_type_column_list(greatest_call->arguments, type, v, TRUE, ensure_same_type, parse_context,
							 NULL);
		if ((!result) && (TABLE_ASTERISK == *type)) {
			ISSUE_TYPE_COMPATIBILITY_ERROR(*type, "greatest operation", &greatest_call->arguments, result);
		}
		break;
	case least_STATEMENT:
		UNPACK_SQL_STATEMENT(least_call, v, least);
		// SqlColumnList
		result
		    |= populate_data_type_column_list(least_call->arguments, type, v, TRUE, ensure_same_type, parse_context, NULL);
		if ((!result) && (TABLE_ASTERISK == *type)) {
			ISSUE_TYPE_COMPATIBILITY_ERROR(*type, "least operation", &least_call->arguments, result);
		}
		break;
	case null_if_STATEMENT:;
		SqlValueType tmp;

		UNPACK_SQL_STATEMENT(null_if, v, null_if);
		// SqlColumnList
		result |= populate_data_type(null_if->left, type, v, parse_context, NULL);
		if (result) {
			break;
		}
		result |= populate_data_type(null_if->right, &tmp, v, parse_context, NULL);
		if (result) {
			break;
		}
		if (BOOLEAN_OR_STRING_LITERAL == *type) {
			SqlValueType fix_type2;

			if (BOOLEAN_OR_STRING_LITERAL == tmp) {
				fix_type2 = STRING_LITERAL;
				result |= populate_data_type(null_if->right, &tmp, v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == tmp);
			} else if (BOOLEAN_VALUE == tmp) {
				fix_type2 = BOOLEAN_VALUE;
			} else {
				fix_type2 = STRING_LITERAL;
			}
			result |= populate_data_type(null_if->left, type, v, parse_context, &fix_type2);
			assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			assert(fix_type2 == *type);
		} else if (BOOLEAN_OR_STRING_LITERAL == tmp) {
			SqlValueType fix_type2;

			if (BOOLEAN_VALUE == *type) {
				fix_type2 = BOOLEAN_VALUE;
			} else {
				fix_type2 = STRING_LITERAL;
			}
			result |= populate_data_type(null_if->right, &tmp, v, parse_context, &fix_type2);
			assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			assert(fix_type2 == tmp);
		}
		result |= ensure_same_type(type, &tmp, null_if->left, null_if->right, parse_context);
		if ((!result) && (TABLE_ASTERISK == *type)) {
			ISSUE_TYPE_COMPATIBILITY_ERROR(*type, "nullif operation", &null_if->left, result);
		}
		break;
	case aggregate_function_STATEMENT:;
		SqlAggregateFunction *aggregate_function;

		UNPACK_SQL_STATEMENT(aggregate_function, v, aggregate_function);
		// SqlColumnList : table.* usage will have more than one node so loop through
		result |= populate_data_type_column_list(aggregate_function->parameter, type, v, TRUE, NULL, parse_context, NULL);
		if (result) {
			break;
		}
		// Note that COUNT(...) is always an INTEGER type even though ... might be a string type column.
		switch (aggregate_function->type) {
		case AGGREGATE_COUNT_DISTINCT:
		case AGGREGATE_COUNT:
			assert(TABLE_ASTERISK != (*type));
			/* The above assert is valid as COUNT(DISTINCT TABLE.*) or COUNT(TABLE.*) value would have been expanded
			 * at qualify_statement() aggregate_function_STATEMENT case to column_list of column_alias values
			 * by "process_aggregate_function_table_asterisk()" call. And this is why we are also guaranteed
			 * that "*type" would be initialized in the "populate_data_type_column_list()" call above as there
			 * is at least one column in the list that would have been processed and "*type" would be initialized
			 * to the type of the last column in that list. For example, in the "AGGREGATE_COUNT_ASTERISK" case
			 * below, we are not guaranteed "*type" is initialized and hence cannot have a similar assert.
			 */
			*type = INTEGER_LITERAL;
			break;
		case AGGREGATE_COUNT_TABLE_ASTERISK:
		case AGGREGATE_COUNT_DISTINCT_TABLE_ASTERISK:
		case AGGREGATE_COUNT_ASTERISK:
			*type = INTEGER_LITERAL;
			break;
		case AGGREGATE_AVG:
		case AGGREGATE_AVG_DISTINCT:
		case AGGREGATE_SUM:
		case AGGREGATE_SUM_DISTINCT:
			if ((TABLE_ASTERISK == *type) || (STRING_LITERAL == *type) || (BOOLEAN_VALUE == *type)
			    || (BOOLEAN_OR_STRING_LITERAL == *type)) {
				/* TABLE_ASTERISK or STRING or BOOLEAN type cannot be input for the AVG or SUM function so signal
				 * an error in that case.
				 */
				if (BOOLEAN_OR_STRING_LITERAL == *type) {
					FIX_TYPE_TO_STRING_LITERAL(*type);
				}
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
			} else if ((AGGREGATE_AVG == aggregate_function->type)
				   || (AGGREGATE_AVG_DISTINCT == aggregate_function->type)) {
				*type = NUMERIC_LITERAL;
			}
			break;
		case AGGREGATE_MIN:
		case AGGREGATE_MAX:
			if ((TABLE_ASTERISK == *type) || (BOOLEAN_VALUE == *type)) {
				/* TABLE_ASTERISK or BOOLEAN type cannot be input for the MIN or MAX function so signal an error in
				 * that case. */
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
			result |= populate_data_type(cur_join->value, type, NULL, parse_context, NULL);
			if (result) {
				break;
			}
			if (NULL != cur_join->condition) {
				result |= populate_data_type(cur_join->condition, type, v, parse_context, NULL);
				if (result) {
					break;
				}
				if (BOOLEAN_OR_STRING_LITERAL == *type) {
					SqlValueType fix_type2;

					fix_type2 = BOOLEAN_VALUE;
					result |= populate_data_type(cur_join->condition, type, v, parse_context, &fix_type2);
					assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd
							    call */
					UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
					assert(fix_type2 == *type);
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
			result |= populate_data_type(value->v.calculated, &child_type[0], v, parse_context, NULL);
			if (result) {
				break;
			}
			*type = child_type[0];
			break;
		case BOOLEAN_OR_STRING_LITERAL:
			if (NULL != fix_type) {
				switch (*fix_type) {
				case BOOLEAN_VALUE:
					FIX_TYPE_TO_BOOLEAN_VALUE(value->type);
					value->v.string_literal = (value->u.bool_or_str.truth_value ? "1" : "0");

					int status;
					status = parse_literal_to_parameter(parse_context, value, TRUE);
					if (0 != status) {
						break;
					}
					break;
				case STRING_LITERAL:
					FIX_TYPE_TO_STRING_LITERAL(value->type);
					break;
				case BOOLEAN_OR_STRING_LITERAL:
					/* This is possible for example in a query [select true,'t' union select 'f','t';]
					 * where while fixing the 'f' (which is a BOOLEAN_OR_STRING_LITERAL) to BOOLEAN_VALUE,
					 * we also try to see if the 't' (2nd column in both operands of the UNION can be
					 * fixed. And both of them end up being BOOLEAN_OR_STRING_LITERAL. In this case,
					 * the 2nd column cla types will be later fixed by "hash_canonical_query" to be
					 * STRING_LITERAL. So do nothing for now.
					 */
					break;
				default:
					assert(FALSE);
					break;
				}
			}
			/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
			/* fall through */
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
			*type = value->type;
			if ((BOOLEAN_VALUE == value->type) && (0 == strlen(value->v.reference))) {
				/* This is an UNKNOWN boolean value (i.e. IS UNKNOWN usage in SQL), which should be treated
				 * the same as SQL NULL after type checking. So, now that we have extracted the BOOLEAN type
				 * for this value, change it to be a regular NUL_VALUE for the purposes of plan generation.
				 */
				value->type = NUL_VALUE;
			}
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
			 * parameter data types have been specified in the "Parse" message. If so use that type instead.
			 */
			*type = PARAMETER_VALUE;
			if (parse_context->is_extended_query) {
				tmp_long = strtol(value->v.string_literal + 1, NULL, 10); /* + 1 to skip '$' prefix in '$1' etc. */
				if ((LONG_MIN != tmp_long) && (LONG_MAX != tmp_long)) {
					param_num = (int)tmp_long;
					parse_context->cur_param_num = param_num;
					/* Check if parse_context->types[param_num - 1] has a non-zero value.
					 * If so, data type information has been specified in the "Parse" message. Use that.
					 * If not, we have no data type information was specified, so skip this step.
					 * See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1236#note_1327912175 for
					 * more details.
					 */
					if ((0 < param_num) && (param_num <= parse_context->num_bind_parm_types)
					    && (0 != parse_context->types[param_num - 1])) {
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
		case COERCE_TYPE:;
			SqlValueType source_type, target_type;

			result |= populate_data_type(value->v.coerce_target, &source_type, v, parse_context, NULL);
			if (result) {
				break;
			}
			target_type = get_sqlvaluetype_from_sqldatatype(value->u.coerce_type.coerced_type.data_type, FALSE);
			if (TABLE_ASTERISK == source_type) {
				/* Type cast of TABLE.* syntax is not allowed as it does not make sense to type cast
				 * what is a record into a scalar type. Issue error.
				 */
				ERROR(ERR_TYPE_CAST, get_user_visible_type_string(source_type),
				      get_user_visible_type_string(target_type));
				yyerror(NULL, NULL, &v, NULL, NULL, NULL);
				result = 1;
				break;
			}
			/* At this time (Sept 2022), we allow any type to be coerced to any other type at parser time (the
			 * exception being TABLE_ASTERISK in which case we would have already issued an error above and
			 * the other is conversion between NUMERIC and BOOLEAN in which case we issue an error below).
			 * Errors in type conversion, if any, should show up at run-time based on the actual values.
			 * src/aux/_ydboctoplanhelpers.m -> ValidateInputAndGetTrimdVal() is invoked in M at run-time to
			 * validate STRING to INTEGER and STRING to NUMERIC conversions and this routine issues a run-time
			 * error if the coercion is invalid.
			 */
			if (((BOOLEAN_VALUE == target_type) && (NUMERIC_LITERAL == source_type))
			    || ((NUMERIC_LITERAL == target_type) && (BOOLEAN_VALUE == source_type))) {
				// Issue error as this is invalid
				ERROR(ERR_TYPE_CAST, get_user_visible_type_string(source_type),
				      get_user_visible_type_string(target_type));
				yyerror(NULL, NULL, &v, NULL, NULL, NULL);
				result = 1;
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == source_type) {
				source_type = STRING_LITERAL;
			}
			/* Note down type of target before coerce */
			value->u.coerce_type.pre_coerced_type = source_type;
			*type = target_type;
			break;
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case SELECT_ASTERISK:
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
			result |= populate_data_type(v->v.column_alias->column, type, v, parse_context, NULL);
			if (result) {
				break;
			}
		} else {
			UNPACK_SQL_STATEMENT(column, v->v.column_alias->column, column);
			*type = get_sqlvaluetype_from_sqldatatype(column->data_type_struct.data_type, FALSE);
		}
		break;
	case column_list_STATEMENT:
		/* Note: We do not loop through the list (just like is done in "hash_canonical_query")
		 * Pass `parent_stmt` itself as the third argument as column_list_STATEMENT represents a list and we are interested
		 * to know who holds the list
		 */
		result |= populate_data_type_column_list(v, type, parent_stmt, FALSE, NULL, parse_context, fix_type);
		break;
	case column_list_alias_STATEMENT:
		/* Note: We do not loop through the list (just like is done in "hash_canonical_query")
		 * Pass `parent_stmt` itself as the third argument as column_list_STATEMENT represents a list and we are interested
		 * to know who holds the list
		 */
		result |= populate_data_type_column_list_alias(v, type, parent_stmt, FALSE, parse_context, NULL);
		break;
	case create_table_STATEMENT:
		assert(NULL == fix_type); /* so no need to handle non-NULL fix_type scenario */
		// Do nothing; we got here through a table_alias
		break;
	case table_value_STATEMENT:
		assert(NULL == fix_type); /* so no need to handle non-NULL fix_type scenario */
		UNPACK_SQL_STATEMENT(table_value, v, table_value);
		/* It is possible we come across an already processed table_value_STATEMENT while processing the `select *`
		 * in below type of queries.
		 *   Example: `select * from (select ((values(lastname))) = 'Burn' from names);`
		 * In such a case, any errors would have already been issued and the statement column data types would have already
		 * been set. Skip further processing in such cases.
		 */
		if (UNKNOWN_SqlDataType != table_value->column->data_type_struct.data_type) {
			*type = get_sqlvaluetype_from_sqldatatype(table_value->column->data_type_struct.data_type, FALSE);
		} else {
			/* For a table constructed using the VALUES clause, go through each value specified and determine
			 * its type. Verify all rows have same type for each column.
			 */
			int colno;

			UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
			start_row_value = row_value;
			num_columns = row_value->num_columns;
			assert(num_columns);

			SqlValueType * type_array;
			boolean_t      saw_boolean_or_string_literal, *saw_boolean_or_string_literal_array;
			SqlStatement **first_value;

			type_array = (SqlValueType *)calloc(num_columns, sizeof(SqlValueType));
			first_value = (SqlStatement **)calloc(num_columns, sizeof(SqlStatement *));
			saw_boolean_or_string_literal_array = (boolean_t *)calloc(num_columns, sizeof(boolean_t));
			assert(FALSE == saw_boolean_or_string_literal_array[0]); /* check that calloc initialized to FALSE */
			saw_boolean_or_string_literal = FALSE;
			do {
				SqlColumnList *start_column_list, *cur_column_list;

				UNPACK_SQL_STATEMENT(start_column_list, row_value->value_list, column_list);
				cur_column_list = start_column_list;
				colno = 0;
				do {
					SqlValueType current_type;

					// SqlValue or SqlColumnAlias
					result |= populate_data_type(cur_column_list->value, &current_type, v, parse_context, NULL);
					if (result) {
						break;
					}
					if (BOOLEAN_OR_STRING_LITERAL == current_type) {
						saw_boolean_or_string_literal_array[colno] = TRUE;
						saw_boolean_or_string_literal = TRUE;
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
			/* Note: Cannot "break" if "result" is non-zero like in other places because we need to do some "free()"
			 * calls */
			if (!result) {
				/* Now that we have scanned all rows of the VALUES table, store the final determined column type.
				 * Note that if one row had a column type of NUL_VALUE and the next row had a type of
				 * NUMERIC_LITERAL, the final column type would end up being NUMERIC_LITERAL.
				 */
				start_column = table_value->column;
				if (saw_boolean_or_string_literal) {
					/* Found at least one column value whose type is BOOLEAN_OR_STRING_LITERAL.
					 * Rescan all the table rows/columns and fix those column values which are of type
					 * BOOLEAN_OR_STRING_LITERAL to be type BOOLEAN_VALUE or STRING_LITERAL as appropriate.
					 */
					row_value = start_row_value;
					do {
						SqlColumnList *start_column_list, *cur_column_list;

						UNPACK_SQL_STATEMENT(start_column_list, row_value->value_list, column_list);
						cur_column_list = start_column_list;
						colno = 0;
						do {
							if (saw_boolean_or_string_literal_array[colno]) {
								SqlValueType fix_type2, type2;

								assert((BOOLEAN_OR_STRING_LITERAL == type_array[colno])
								       || (STRING_LITERAL == type_array[colno])
								       || (BOOLEAN_VALUE == type_array[colno]));
								fix_type2 = (BOOLEAN_OR_STRING_LITERAL == type_array[colno])
										? STRING_LITERAL
										: type_array[colno];
								result |= populate_data_type(cur_column_list->value, &type2, v,
											     parse_context, &fix_type2);
								assert(!result); /* type fixing call of "populate_data_type" should
										    never fail as it is 2nd call */
								UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores]
										    warning */
								assert((fix_type2 == type2) || IS_NUL_VALUE(type2));
							}
							colno++;
							cur_column_list = cur_column_list->next;
						} while ((cur_column_list != start_column_list));
						row_value = row_value->next;
					} while (row_value != start_row_value);
					/* If there are columns whose types are still ambiguous, reset them to STRING */
					column = start_column;
					colno = 0;
					do {
						if (BOOLEAN_OR_STRING_LITERAL == type_array[colno]) {
							FIX_TYPE_TO_STRING_LITERAL(type_array[colno]);
						}
						column = column->next;
						colno++;
					} while (column != start_column);
				}
				column = start_column;
				colno = 0;
				do {
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
			free(saw_boolean_or_string_literal_array);
			free(type_array);
			free(first_value);
		}
		assert(BOOLEAN_OR_STRING_LITERAL != *type);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, v, table_alias);
		if (NULL == fix_type) {
			ISSUE_ERR_AND_BREAK_IF_SUBQUERY_HAS_MULTIPLE_COLUMNS(v, parent_stmt, result);
		} else {
			/* For example, we are being called from a boolean operation whose one side involves a BOOLEAN_VALUE
			 * or STRING_LITERAL operand and other side involves a sub-query operand that returns the
			 * BOOLEAN_OR_STRING_LITERAL type.  In that case, the boolean operation will invoke "populate_data_type"
			 * on the sub-query again to fix its type to the type of the other side of the boolean operation.
			 * In that case, we are crossing a sub-query boundary and so we are going to fix the type of the
			 * sub-query to STRING_LITERAL and not BOOLEAN_VALUE. The caller will later issue a type mismatch error.
			 */
			assert(select_STATEMENT == table_alias->table->type);
			assert((STRING_LITERAL == *fix_type) || (BOOLEAN_VALUE == *fix_type));

			SqlColumnListAlias fix_type_cla;
			fix_type_cla.type = STRING_LITERAL; /* this is the only field that the below call relies on
							     * so we leave the other fields in the structure uninitialized.
							     */
			result = populate_data_type_cla_fix(v, parse_context, &fix_type_cla);
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
			*type = fix_type_cla.type;
			break;
		}
		result |= populate_data_type(table_alias->table, type, parent_stmt, parse_context, NULL);
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
			result |= populate_data_type_column_list_alias(table_alias->column_list, &child_type[0], parent_stmt, TRUE,
								       parse_context, NULL);
		}
		break;
	case binary_STATEMENT:;
		SqlBinaryOperation *binary;
		boolean_t	    is_in_column_list;

		UNPACK_SQL_STATEMENT(binary, v, binary);
		result |= populate_data_type(binary->operands[0], &child_type[0], v, parse_context, NULL);
		if (result) {
			break;
		}
		if (((BOOLEAN_IN == binary->operation) || (BOOLEAN_NOT_IN == binary->operation))
		    && (column_list_STATEMENT == binary->operands[1]->type)) { // SqlColumnList
			result |= populate_data_type_column_list(binary->operands[1], &child_type[1], v, TRUE, ensure_same_type,
								 parse_context, NULL);
			is_in_column_list = TRUE;
		} else {
			// SqlStatement (?)
			result |= populate_data_type(binary->operands[1], &child_type[1], v, parse_context, NULL);
			is_in_column_list = FALSE;
		}
		if (result) {
			break;
		}
		if (BOOLEAN_OR_STRING_LITERAL == child_type[0]) {
			SqlValueType fix_type2;

			if (BOOLEAN_OR_STRING_LITERAL == child_type[1]) {
				/* Note: The below "switch" is modeled on the switch in "binary_operation_data_type_check.c".
				 * Any additions to binary operations will involve a new "case" block below since there is
				 * no "default:" case block (intentionally not there so compiler warns about new missing cases
				 * instead of hiding subtle bugs by going through default: code path).
				 */
				switch (binary->operation) {
				case BOOLEAN_OR:
				case BOOLEAN_AND:
				case BOOLEAN_IS:
				case BOOLEAN_IS_NOT:
					fix_type2 = BOOLEAN_VALUE;
					break;
				case BOOLEAN_EQUALS:
				case BOOLEAN_NOT_EQUALS:
				case BOOLEAN_LESS_THAN:
				case BOOLEAN_GREATER_THAN:
				case BOOLEAN_LESS_THAN_OR_EQUALS:
				case BOOLEAN_GREATER_THAN_OR_EQUALS:
				case BOOLEAN_REGEX_SENSITIVE:
				case BOOLEAN_REGEX_INSENSITIVE:
				case BOOLEAN_REGEX_SENSITIVE_LIKE:
				case BOOLEAN_REGEX_INSENSITIVE_LIKE:
				case BOOLEAN_REGEX_SENSITIVE_SIMILARTO:
				case BOOLEAN_REGEX_INSENSITIVE_SIMILARTO:
				case BOOLEAN_IN:
				case BOOLEAN_NOT_IN:
				case BOOLEAN_ANY_EQUALS:
				case BOOLEAN_ANY_NOT_EQUALS:
				case BOOLEAN_ANY_LESS_THAN:
				case BOOLEAN_ANY_GREATER_THAN:
				case BOOLEAN_ANY_LESS_THAN_OR_EQUALS:
				case BOOLEAN_ANY_GREATER_THAN_OR_EQUALS:
				case BOOLEAN_ALL_EQUALS:
				case BOOLEAN_ALL_NOT_EQUALS:
				case BOOLEAN_ALL_LESS_THAN:
				case BOOLEAN_ALL_GREATER_THAN:
				case BOOLEAN_ALL_LESS_THAN_OR_EQUALS:
				case BOOLEAN_ALL_GREATER_THAN_OR_EQUALS:
				case ADDITION:
				case SUBTRACTION:
				case DIVISION:
				case MULTIPLICATION:
				case MODULO:;
				case CONCAT:
					fix_type2 = STRING_LITERAL;
					break;
				}
				result |= populate_data_type(binary->operands[0], &child_type[0], v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[0]);
				if (is_in_column_list) {
					result |= populate_data_type_column_list(binary->operands[1], &child_type[1], v, TRUE, NULL,
										 parse_context, &fix_type2);
				} else {
					result |= populate_data_type(binary->operands[1], &child_type[1], v, parse_context,
								     &fix_type2);
				}
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[1]);
			} else {
				if (BOOLEAN_VALUE == child_type[1]) {
					fix_type2 = BOOLEAN_VALUE;
				} else {
					fix_type2 = STRING_LITERAL;
				}
				result |= populate_data_type(binary->operands[0], &child_type[0], v, parse_context, &fix_type2);
				assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[0]);
			}
		} else if (BOOLEAN_OR_STRING_LITERAL == child_type[1]) {
			SqlValueType fix_type2;

			if (BOOLEAN_VALUE == child_type[0]) {
				fix_type2 = BOOLEAN_VALUE;
			} else {
				fix_type2 = STRING_LITERAL;
			}
			if (is_in_column_list) {
				result |= populate_data_type_column_list(binary->operands[1], &child_type[1], v, TRUE, NULL,
									 parse_context, &fix_type2);
			} else {
				result |= populate_data_type(binary->operands[1], &child_type[1], v, parse_context, &fix_type2);
			}
			assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			/* Note that if binary->operands[1] points to a sub-query, then the above would have fixed the
			 * BOOLEAN_OR_STRING_LITERAL type to be STRING_LITERAL irrespective of what "fix_type2" was.
			 * Hence the below assert.
			 */
			assert((fix_type2 == child_type[1]) || (STRING_LITERAL == child_type[1]));
		}
		assert(BOOLEAN_OR_STRING_LITERAL != child_type[0]);
		assert(BOOLEAN_OR_STRING_LITERAL != child_type[1]);
		result = binary_operation_data_type_check(binary, child_type, type, parse_context);
		break;
	case set_operation_STATEMENT:
		if (NULL != fix_type) {
			/* See comments in similar code in "case table_alias_STATEMENT:" case
			 * for more details on why this code is needed.
			 */
			SqlColumnListAlias fix_type_cla;
			fix_type_cla.type = STRING_LITERAL; /* this is the only field that the below call relies on
							     * so we leave the other fields in the structure uninitialized.
							     */
			result = populate_data_type_cla_fix(v, parse_context, &fix_type_cla);
			assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			*type = fix_type_cla.type;
			break;
		}
		ISSUE_ERR_AND_BREAK_IF_SUBQUERY_HAS_MULTIPLE_COLUMNS(v, parent_stmt, result);
		UNPACK_SQL_STATEMENT(set_operation, v, set_operation);
		result |= populate_data_type(set_operation->operand[0], &child_type[0], parent_stmt, parse_context, NULL);
		if (result) {
			break;
		}
		result |= populate_data_type(set_operation->operand[1], &child_type[1], parent_stmt, parse_context, NULL);
		if (result) {
			break;
		}
		/* Now that the types of operands to the SET operation have been populated, do some more checks of
		 * whether the # and types of columns on both operands match. If not issue error.
		 */
		result |= check_column_lists_for_type_match(v, parse_context);
		if (result) {
			break;
		}
		*type = child_type[0];
		CAST_AMBIGUOUS_TYPES(*type, child_type[1], result, parse_context);
		assert(!result); /* the above CAST_AMBIGUOUS_TYPES call should have not errored out */
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;
		boolean_t	   is_boolean_or_string_literal;

		UNPACK_SQL_STATEMENT(unary, v, unary);
		result |= populate_data_type(unary->operand, &child_type[0], v, parse_context, NULL);
		if (result) {
			break;
		}
		is_boolean_or_string_literal = (BOOLEAN_OR_STRING_LITERAL == child_type[0]);
		result = unary_operation_data_type_check(unary, child_type, type);
		if (result) {
			break;
		}
		assert(BOOLEAN_OR_STRING_LITERAL != *type);
		if (is_boolean_or_string_literal) {
			/* The call to "unary_operation_data_type_check()" fixed BOOLEAN_OR_STRING_LITERAL type to BOOLEAN_VALUE
			 * for the BOOLEAN_NOT case. Reinvoke "populate_data_type()" to fix the unary operand type.
			 */
			assert(BOOLEAN_VALUE == *type);
			result |= populate_data_type(unary->operand, &child_type[0], v, parse_context, type);
			assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
		}
		break;
	case create_function_STATEMENT:
	case drop_table_STATEMENT:
	case drop_function_STATEMENT:
	case truncate_table_STATEMENT:
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
	case display_relation_STATEMENT:
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
