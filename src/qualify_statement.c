/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Following macro sets GROUP BY column number to expression when its matched with GROUP BY column.
 * Also, it `break`s when no additional processing is required.
 */
#define SET_GROUP_BY_EXPRESSION_COLUMN_NUMBER_AND_BREAK(SQL_STMT, TABLE_ALIAS)                                                     \
	{                                                                                                                          \
		/* We only need to qualify expressions when `table_alias->do_group_by_checks` is TRUE and GROUP BY exists. */      \
		/* Also, use `aggregate_depth` to identify and process columns of only `SELECT`, `HAVING` and `ORDER BY` clause */ \
		if ((GROUP_BY_SPECIFIED & TABLE_ALIAS->aggregate_function_or_group_by_or_having_specified)                         \
		    && TABLE_ALIAS->do_group_by_checks                                                                             \
		    && ((0 == TABLE_ALIAS->aggregate_depth) || (AGGREGATE_DEPTH_HAVING_CLAUSE == TABLE_ALIAS->aggregate_depth))) { \
			group_by_fields_t *gb_fields = get_group_by_fields(SQL_STMT);                                              \
			if (NULL != gb_fields) {                                                                                   \
				int column_number = get_group_by_column_number(TABLE_ALIAS, SQL_STMT);                             \
				if (-1 != column_number) {                                                                         \
					gb_fields->group_by_column_num = column_number;                                            \
					/* This expression is part of GROUP BY. GROUP BY has already gone through                  \
					 * qualify_statement() for this expression till its leaf nodes and any errors in it has    \
					 * already been reported. Hence we prevent further qualify_statement() calls by doing a    \
					 * break here. */                                                                          \
					break;                                                                                     \
				} /* else drill down to deeper level in the expression sub-tree for GROUP BY expression matching   \
				   */                                                                                              \
			}                                                                                                          \
		}                                                                                                                  \
	}

/* Note: The code in "qualify_check_constraint.c" is modeled on the below so it is possible changes here might need to be
 *       made there too. And vice versa (i.e. changes to "qualify_statement.c" might need to be made here too).
 *       An automated tool "tools/ci/check_code_base_assertions.csh" alerts us (through the pre-commit script and/or
 *       pipeline jobs) if these two get out of sync.
 */

/* Returns:
 *	0 if query is successfully qualified.
 *	1 if query had errors during qualification.
 */
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *table_alias_stmt, int depth, QualifyStatementParms *ret) {
	SqlBinaryOperation *	binary;
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlCaseStatement *	cas;
	SqlColumnAlias *	new_column_alias;
	SqlColumnList *		start_cl, *cur_cl;
	SqlColumnListAlias *	start_cla, *cur_cla;
	SqlFunctionCall *	fc;
	SqlCoalesceCall *	coalesce_call;
	SqlGreatest *		greatest_call;
	SqlLeast *		least_call;
	SqlNullIf *		null_if;
	SqlUnaryOperation *	unary;
	SqlArray *		array;
	SqlValue *		value;
	int			result;
	SqlTableAlias *		column_table_alias, *parent_table_alias, *table_alias;
	SqlColumnListAlias **	ret_cla;
	int			save_max_unique_id;
	int			i;

	result = 0;
	if (NULL == stmt)
		return result;
	if ((NULL != ret) && (NULL != ret->max_unique_id)) {
		/* Determine max_unique_id under current subtree and store it in "stmt->hash_canonical_query_cycle".
		 * Hence the temporary reset. At the end, before returning, we will update "ret->max_unique_id" to be MAX.
		 */
		save_max_unique_id = *ret->max_unique_id;
		assert(0 <= save_max_unique_id);
		*ret->max_unique_id = 0;
	} else {
		save_max_unique_id = 0;
	}
	switch (stmt->type) {
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(new_column_alias, stmt, column_alias);
		UNPACK_SQL_STATEMENT(column_table_alias, new_column_alias->table_alias_stmt, table_alias);
		parent_table_alias = column_table_alias->parent_table_alias;
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		/* Assert that if we are doing GROUP BY related checks ("do_group_by_checks" is set to DO_GROUP_BY_CHECKS), then the
		 * "aggregate_function_or_group_by_or_having_specified" field is also TRUE.
		 */
		assert(!parent_table_alias->do_group_by_checks
		       || parent_table_alias->aggregate_function_or_group_by_or_having_specified);
		/*
		 * This block is used in two cases
		 * 1. GroupBy is used and we need to validate column_alias usages
		 * 2. In case GroupBy is not present but HAVING clause is present. We still want to generate an error on
		 *    column_alias usages as they are not grouped. To allow such condition check this case is used to perform the
		 *    necessary validation.
		 */
		// 1. Column Reference check
		if ((parent_table_alias->do_group_by_checks)
		    && ((0 == parent_table_alias->aggregate_depth)
			|| (AGGREGATE_DEPTH_HAVING_CLAUSE == parent_table_alias->aggregate_depth))
		    && !new_column_alias->group_by_column_number) {
			/* 1) We are doing GROUP BY related validation of column references in the query
			 * 2) The current column reference is not inside an aggregate function
			 * 3) The current column reference is not in the GROUP BY clause.
			 * Issue an error.
			 */
			SqlStatement *column_name;
			SqlValue *    value;
			column_name = find_column_alias_name(stmt);
			assert(NULL != column_name);
			UNPACK_SQL_STATEMENT(value, column_name, value);
			ERROR(ERR_GROUP_BY_OR_AGGREGATE_FUNCTION, value->v.string_literal);
			yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
			result = 1;
		}
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case TABLE_ASTERISK:
			/* Any usage of table.* in a query leads to this case. It needs to be qualified similar to COLUMN_REFERENCE
			 * because later stages will assume this type is placed in a COLUMN_ALIAS and has the necessary
			 * table_alias_STATEMENT.
			 */
		case COLUMN_REFERENCE:
			UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
			if (table_alias->do_group_by_checks) {
				/* This is the second pass. Any appropriate error has already been issued so skip this. */
				break;
			}
			ret_cla = ((NULL == ret) ? NULL : ret->ret_cla);
			new_column_alias = qualify_column_name(value, tables, table_alias_stmt, depth + 1, ret_cla);
			result = ((NULL == new_column_alias) && ((NULL == ret_cla) || (NULL == *ret_cla)));
			if (result) {
				yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
			} else {
				if ((NULL == ret_cla) || (NULL == *ret_cla)) {
					// Convert this statement to a qualified one by changing the "type".
					// Note: "qualify_column_name.c" relies on the below for qualifying column names
					stmt->type = column_alias_STATEMENT;
					stmt->v.column_alias = new_column_alias;
					UNPACK_SQL_STATEMENT(column_table_alias, new_column_alias->table_alias_stmt, table_alias);
					if ((NULL != ret) && (NULL != ret->max_unique_id)) {
						int *max_unique_id;

						max_unique_id = ret->max_unique_id;
						if (*max_unique_id <= column_table_alias->unique_id) {
							*max_unique_id = column_table_alias->unique_id + 1;
						}
					}
					parent_table_alias = column_table_alias->parent_table_alias;
					if ((NULL != parent_table_alias) && (parent_table_alias->aggregate_depth)) {
						if ((0 < table_alias->aggregate_depth)
						    && (QualifyQuery_WHERE == parent_table_alias->qualify_query_stage)) {
							/* `aggregate_function_STATEMENT` case itself throws error when current
							 * table_alias is executing a WHERE clause. Because of that we are sure we
							 * wont reach this block of code when `table_alias == parent_table_alias`.
							 */
							assert(table_alias != parent_table_alias);
							/* Aggregate functions in a sub query is referencing outer query
							 * column. Also, the sub query is inside WHERE clause of the outer
							 * query. This usage is not allowed. Issue an ERROR.
							 */
							ERROR(ERR_AGGREGATE_FUNCTION_WHERE, "");
							result = 1;
							yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
						}
						int aggregate_depth = parent_table_alias->aggregate_depth;
						if (0 < aggregate_depth) {
							assert(AGGREGATE_FUNCTION_SPECIFIED
							       & parent_table_alias
								     ->aggregate_function_or_group_by_or_having_specified);
						} else if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == aggregate_depth) {
							if (QualifyQuery_GROUP_BY_EXPRESSION != table_alias->qualify_query_stage) {
								/* Update `group_by_column_count` and `group_by_column_number` */
								new_column_alias->group_by_column_number
								    = ++parent_table_alias->group_by_column_count;
							} else {
								/* 1. This column alias is part of an expression in GroupBy.
								 * 2. Do not update `group_by_column_number` here as
								 *    get_group_by_column_num() takes care of it later.
								 * 3. Do not update `group_by_column_count` here as we want to
								 *    increment this value by one for the entire expression. This
								 *    is taken care at the end of `column_list_alias_STATEMENT`.
								 */
							}
						} else {
							/* We are inside a WHERE or FROM/JOIN clause where  aggregate
							 * function use is disallowed so no need to record anything related
							 * to GROUP BY.
							 */
							assert((AGGREGATE_DEPTH_WHERE_CLAUSE == aggregate_depth)
							       || (AGGREGATE_DEPTH_FROM_CLAUSE == aggregate_depth)
							       || (AGGREGATE_DEPTH_HAVING_CLAUSE));
						}
					}
				} else {
					/* Return pointer type is SqlColumnListAlias (not SqlColumnAlias).
					 * In this case, the needed adjustment of data type will be done in an ancestor
					 * caller of "qualify_statement" in the "case column_list_alias_STATEMENT:" code
					 * block (look for "QUALIFY_COLUMN_REFERENCE" in a comment in that code block).
					 * So just return.
					 */
				}
			}
			break;
		case CALCULATED_VALUE:
			UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
			SET_GROUP_BY_EXPRESSION_COLUMN_NUMBER_AND_BREAK(stmt, table_alias);
			result |= qualify_statement(value->v.calculated, tables, table_alias_stmt, depth + 1, ret);
			break;
		case FUNCTION_NAME:
			/* Cannot validate the function using a "find_function()" call here (like we did "find_table()" for
			 * the table name in the parser). This is because we need type information of the actual function
			 * parameters to determine which function definition matches the current usage and that has to wait
			 * until "populate_data_type()". See detailed comment under "case function_call_STATEMENT:" there.
			 */
			break;
		case COERCE_TYPE:
			UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
			SET_GROUP_BY_EXPRESSION_COLUMN_NUMBER_AND_BREAK(stmt, table_alias);
			result |= qualify_statement(value->v.coerce_target, tables, table_alias_stmt, depth + 1, ret);
			if (result) {
				yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
				break;
			}
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
		case PARAMETER_VALUE:
			assert(0 == value->group_by_fields.group_by_column_num);
			break;
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
			assert(FALSE);
			break;
			/* Do not add "default" case as we want to enumerate each explicit case here instead of having a
			 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
			 */
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		SET_GROUP_BY_EXPRESSION_COLUMN_NUMBER_AND_BREAK(stmt, table_alias);
		for (i = 0; i < 2; i++) {
			result |= qualify_statement(binary->operands[i], tables, table_alias_stmt, depth + 1, ret);
		}
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		SET_GROUP_BY_EXPRESSION_COLUMN_NUMBER_AND_BREAK(stmt, table_alias);
		result |= qualify_statement(unary->operand, tables, table_alias_stmt, depth + 1, ret);
		break;
	case array_STATEMENT:
		UNPACK_SQL_STATEMENT(array, stmt, array);
		result |= qualify_statement(array->argument, tables, table_alias_stmt, depth + 1, ret);
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		result |= qualify_statement(fc->function_name, tables, table_alias_stmt, depth + 1, ret);
		result |= qualify_statement(fc->parameters, tables, table_alias_stmt, depth + 1, ret);
		break;
	case coalesce_STATEMENT:
		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		result |= qualify_statement(coalesce_call->arguments, tables, table_alias_stmt, depth + 1, ret);
		break;
	case greatest_STATEMENT:
		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		result |= qualify_statement(greatest_call->arguments, tables, table_alias_stmt, depth + 1, ret);
		break;
	case least_STATEMENT:
		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		result |= qualify_statement(least_call->arguments, tables, table_alias_stmt, depth + 1, ret);
		break;
	case null_if_STATEMENT:
		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		result |= qualify_statement(null_if->left, tables, table_alias_stmt, depth + 1, ret);
		result |= qualify_statement(null_if->right, tables, table_alias_stmt, depth + 1, ret);
		break;
	case aggregate_function_STATEMENT:;
		SqlAggregateFunction *af;
		boolean_t	      depth_adjusted;

		UNPACK_SQL_STATEMENT(af, stmt, aggregate_function);
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		depth_adjusted = FALSE;
		if (table_alias->aggregate_depth) {
			if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth) {
				/* 1. We can reach this block if an aggregate function from select list is referenced
				 *    using column number. Ex: `select count(n1.*) from names group by 1;`
				 * 2. We only get to know of this usage during second qualify_statement() call which happens
				 *    after GROUP BY column number is replaced with the SELECT list column in
				 *    `column_list_alias_STATEMENT` case block, so we need this check.
				 * 3. GROUP BY specified uses an aggregate function. Issue error as this usage is not
				 *    valid.
				 */
				ERROR(ERR_GROUP_BY_INVALID_USAGE, "");
				yyerror(NULL, NULL, &af->parameter, NULL, NULL, NULL);
				result = 1;
				break;
			}
			if (table_alias->do_group_by_checks) {
				/* This is the second pass. Any appropriate error other than the one in IF block
				 * has already been issued so skip this.
				 */
				break;
			}
			if (0 < table_alias->aggregate_depth) {
				/* Nesting of aggregate functions at the same query level is not allowed */
				ERROR(ERR_AGGREGATE_FUNCTION_NESTED, "");
				result = 1;
			} else {
				/* Note: AGGREGATE_DEPTH_GROUP_BY_CLAUSE is also negative but we should never get here in that
				 *       case as the parser for GROUP BY would have issued an error if ever GROUP BY is
				 *       used without just a plain column name.
				 */
				if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth) {
					/* GROUP BY specified using an aggregate function. Issue error as this usage is not valid.
					 */
					ERROR(ERR_GROUP_BY_INVALID_USAGE, "");
				} else if (AGGREGATE_DEPTH_WHERE_CLAUSE == table_alias->aggregate_depth) {
					/* Aggregate functions are not allowed inside a WHERE clause */
					ERROR(ERR_AGGREGATE_FUNCTION_WHERE, "");
					result = 1;
				} else if (AGGREGATE_DEPTH_FROM_CLAUSE == table_alias->aggregate_depth) {
					/* Aggregate functions are not allowed inside a FROM clause.
					 * Since the only way to use aggregate functions inside a FROM clause is through
					 * JOIN conditions, issue a JOIN related error (not a FROM related error).
					 */
					ERROR(ERR_AGGREGATE_FUNCTION_JOIN, "");
					result = 1;
				} else if (AGGREGATE_DEPTH_HAVING_CLAUSE == table_alias->aggregate_depth) {
					/* Aggregate functions are allowed inside a HAVING clause.
					 * Temporarily reset depth to 0 before going inside the aggregate function call.
					 */
					table_alias->aggregate_depth = 0;
					depth_adjusted = TRUE;
				} else {
					assert(FALSE);
				}
			}
			if (result) {
				yyerror(&af->parameter->loc, NULL, NULL, NULL, NULL, NULL);
			}
		}
		if (!result) {
			assert(!table_alias->do_group_by_checks || table_alias->aggregate_function_or_group_by_or_having_specified);
			table_alias->aggregate_function_or_group_by_or_having_specified |= AGGREGATE_FUNCTION_SPECIFIED;
			table_alias->aggregate_depth++;
			result |= qualify_statement(af->parameter, tables, table_alias_stmt, depth + 1, ret);
			if (0 == result) {
				UNPACK_SQL_STATEMENT(cur_cl, af->parameter, column_list);
				assert((AGGREGATE_COUNT_ASTERISK == af->type) || (NULL != cur_cl->value));
				assert((AGGREGATE_COUNT_ASTERISK != af->type) || (NULL == cur_cl->value));
				if ((NULL != cur_cl->value) && (column_alias_STATEMENT == cur_cl->value->type)) {
					UNPACK_SQL_STATEMENT(new_column_alias, cur_cl->value, column_alias);
					if (is_stmt_table_asterisk(new_column_alias->column)) {
						process_aggregate_function_table_asterisk(af);
					}
				}
			}
			table_alias->aggregate_depth--;
		}
		if (depth_adjusted) {
			/* Undo temporary reset of depth now that we have finished qualifying the aggregate function call */
			table_alias->aggregate_depth = AGGREGATE_DEPTH_HAVING_CLAUSE;
		}
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		SET_GROUP_BY_EXPRESSION_COLUMN_NUMBER_AND_BREAK(stmt, table_alias);
		result |= qualify_statement(cas->value, tables, table_alias_stmt, depth + 1, ret);
		result |= qualify_statement(cas->branches, tables, table_alias_stmt, depth + 1, ret);
		result |= qualify_statement(cas->optional_else, tables, table_alias_stmt, depth + 1, ret);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		do {
			result |= qualify_statement(cur_branch->condition, tables, table_alias_stmt, depth + 1, ret);
			result |= qualify_statement(cur_branch->value, tables, table_alias_stmt, depth + 1, ret);
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			result |= qualify_statement(cur_cl->value, tables, table_alias_stmt, depth + 1, ret);
			cur_cl = cur_cl->next;
		} while (cur_cl != start_cl);
		break;
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth) {
			ERROR(ERR_GROUP_BY_SUB_QUERY, "");
			yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
			result = 1;
			break;
		}
		result |= qualify_query(stmt, tables, table_alias, ret);
		break;
	case column_list_alias_STATEMENT:;
		boolean_t function_expression = FALSE;
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		cur_cla = start_cla;
		do {
			ret_cla = ((NULL == ret) ? NULL : ret->ret_cla);
			assert(depth || (NULL == ret_cla)
			       || (NULL == *ret_cla)); /* assert that caller has initialized "*ret_cla" */
			UNPACK_SQL_STATEMENT(cur_cl, cur_cla->column_list, column_list);
			if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth) {
				/* Set QualifyQuery_GROUP_BY_EXPRESSION here so that all qualify_statement() calls
				 * for `cur_cl->value` can know that an expression is being qualified.
				 */
				if (value_STATEMENT == cur_cl->value->type) {
					UNPACK_SQL_STATEMENT(value, cur_cl->value, value);
					if ((COLUMN_REFERENCE != value->type) && (TABLE_ASTERISK != value->type)) {
						table_alias->qualify_query_stage = QualifyQuery_GROUP_BY_EXPRESSION;
						function_expression = TRUE;
					}
				} else if (column_alias_STATEMENT != cur_cl->value->type) {
					/* This is an expression, following assignment prevents updation
					 * to `group_by_column_count` of `table_alias` for every column
					 * reference in the expression. The field is updated once at the end
					 * of this case block.
					 */
					table_alias->qualify_query_stage = QualifyQuery_GROUP_BY_EXPRESSION;
					function_expression = TRUE;
				} /* else column_alias_STATEMENT is possible when subquery GROUP BY references outer query
				   * column. If the outer query column is already qualified then we can get this value.
				   * Since this is a column reference no need to updated `qualify_query_stage`.
				   */
			}
			result |= qualify_statement(cur_cla->column_list, tables, table_alias_stmt, depth + 1, ret);
			if (function_expression) {
				function_expression = FALSE;
				table_alias->qualify_query_stage = QualifyQuery_NONE;
			}
			if (0 == result) {
				if (column_alias_STATEMENT == cur_cl->value->type) {
					UNPACK_SQL_STATEMENT(new_column_alias, cur_cl->value, column_alias);
					if (is_stmt_table_asterisk(new_column_alias->column)) {
						process_table_asterisk_cla(stmt, &cur_cla, &start_cla,
									   table_alias->qualify_query_stage);
					}
				}
			}
			/* Check if its an OrderBy or GroupBy invocation */
			if (((NULL != ret_cla) || (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth)) && (0 == depth)
			    && (!table_alias->do_group_by_checks)) {
				SqlColumnListAlias *qualified_cla;
				int		    column_number;
				char *		    str;
				boolean_t	    order_by_alias;

				/* "ret_cla" is non-NULL implies this call is for an ORDER BY column list.
				 * "depth" == 0 implies "cur_cla" is one of the ORDER BY columns
				 * (i.e. not a cla corresponding to an inner evaluation in the ORDER BY expression).
				 * There are 3 cases to handle.
				 */
				if ((NULL != ret_cla) && (NULL != *ret_cla)) {
					/* Case (1) : If "*ret_cla" is non-NULL, it is a case of ORDER BY using an alias name */

					order_by_alias = TRUE;
					/* "QUALIFY_COLUMN_REFERENCE" : This code block finishes the column name validation
					 * that was not finished in the "qualify_column_name" call done in the
					 * "case COLUMN_REFERENCE:" code block above. We found another existing cla that is
					 * already qualified and matches the current non-qualified cla. So fix the current
					 * cla to mirror the already qualified cla.
					 */
					qualified_cla = *ret_cla;
					*ret_cla = NULL; /* initialize for next call to "qualify_statement" */
				} else {
					/* Case (2) : Check if this is a case of column-number usage.
					 * If so, point "cur_cla" to corresponding cla from the SELECT column list.
					 */
					SqlColumnList *col_list;
					boolean_t      error_encountered = FALSE;
					boolean_t      is_positive_numeric_literal, is_negative_numeric_literal;

					order_by_alias = FALSE;
					UNPACK_SQL_STATEMENT(col_list, cur_cla->column_list, column_list);
					/* Check for positive numeric literal */
					is_positive_numeric_literal = ((value_STATEMENT == col_list->value->type)
								       && ((INTEGER_LITERAL == col_list->value->v.value->type)
									   || (NUMERIC_LITERAL == col_list->value->v.value->type)));
					/* Check for negative numeric literal next */
					is_negative_numeric_literal = FALSE;
					if (!is_positive_numeric_literal) {
						if (unary_STATEMENT == col_list->value->type) {
							SqlUnaryOperation *unary;

							UNPACK_SQL_STATEMENT(unary, col_list->value, unary);
							is_negative_numeric_literal
							    = ((NEGATIVE == unary->operation)
							       && (value_STATEMENT == unary->operand->type)
							       && ((INTEGER_LITERAL == unary->operand->v.value->type)
								   || (NUMERIC_LITERAL == unary->operand->v.value->type)));
							if (is_negative_numeric_literal) {
								str = unary->operand->v.value->v.string_literal;
							}
						}
					} else {
						str = col_list->value->v.value->v.string_literal;
					}
					if (is_positive_numeric_literal || is_negative_numeric_literal) {
						/* Check if numeric literal is an integer. We are guaranteed by the
						 * lexer that this numeric literal only contains digits [0-9] and
						 * optionally a '.'. Check if the '.' is present. If so issue error
						 * as decimal column numbers are disallowed in ORDER BY.
						 */
						char *	 ptr, *ptr_top;
						long int retval;

						for (ptr = str, ptr_top = str + strlen(str); ptr < ptr_top; ptr++) {
							if ('.' == *ptr) {
								ERROR(ERR_ORDER_BY_POSITION_NOT_INTEGER,
								      is_negative_numeric_literal ? "-" : "", str);
								yyerror(NULL, NULL, &cur_cla->column_list, NULL, NULL, NULL);
								error_encountered = 1;
								break;
							}
						}
						if (!error_encountered) {
							/* Now that we have confirmed the string only consists of the digits [0-9],
							 * check if it is a valid number that can be represented in an integer.
							 * If not issue error.
							 */
							retval = strtol(str, NULL, 10);
							if ((LONG_MIN == retval) || (LONG_MAX == retval)) {
								ERROR(ERR_ORDER_BY_POSITION_NOT_INTEGER,
								      is_negative_numeric_literal ? "-" : "", str);
								yyerror(NULL, NULL, &cur_cla->column_list, NULL, NULL, NULL);
								error_encountered = 1;
							}
						}
						if (!error_encountered) {
							/* Now that we have confirmed the input string is a valid integer,
							 * check if it is within the range of valid column numbers in the
							 * SELECT column list. If not, issue an error. If we already determined
							 * that this is a negative numeric literal, we can issue an error without
							 * looking at anything else. We wait for the decimal and integer range
							 * checks above to mirror the error messages that Postgres does.
							 */
							if (!is_negative_numeric_literal) {
								column_number = (int)retval;
								qualified_cla = get_column_list_alias_n_from_table_alias(
								    table_alias, column_number);
							} else {
								qualified_cla = NULL;
							}
							if (NULL == qualified_cla) {
								if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE
								    == table_alias->aggregate_depth) {
									ERROR(ERR_GROUP_BY_POSITION_INVALID,
									      is_negative_numeric_literal ? "-" : "", str);
								} else {
									ERROR(ERR_ORDER_BY_POSITION_INVALID,
									      is_negative_numeric_literal ? "-" : "", str);
								}
								yyerror(NULL, NULL, &cur_cla->column_list, NULL, NULL, NULL);
								error_encountered = 1;
							}
						}
						if (error_encountered) {
							result = 1;
							break;
						}
					} else {
						qualified_cla = NULL;
					}
				}
				/* Actual repointing to SELECT column list cla happens here (above code set things up for here) */
				if (NULL != qualified_cla) {
					/* Case (2) : Case of column-number */
					cur_cla->column_list = qualified_cla->column_list;
					assert(NULL == cur_cla->alias);
					/* Note: It is not necessary to copy " qualified_cla->alias" into "cur_cla->alias" */
					/* "cur_cla->keywords" might have ASC or DESC keywords (for ORDER BY)
					 * which the qualified_cla would not have so do not overwrite those.
					 */
					cur_cla->type = qualified_cla->type;
					/* Set unique_id and column_number fields. Assert that they are non-zero as
					 * "hash_canonical_query" skips hashing these fields if they are 0.
					 */
					cur_cla->tbl_and_col_id.unique_id = table_alias->unique_id;
					assert(cur_cla->tbl_and_col_id.unique_id);
					if (order_by_alias) {
						/* Find the column number from the matched cla */
						cur_cla->tbl_and_col_id.column_number
						    = get_column_number_from_column_list_alias(qualified_cla, table_alias);
					} else {
						/* The column number was already specified. So use that as is. */
						cur_cla->tbl_and_col_id.column_number = column_number;
					}
					assert(cur_cla->tbl_and_col_id.column_number);
					if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth) {
						// call qualify_statement again so that GROUP BY is validated
						result |= qualify_statement(cur_cla->column_list, tables, table_alias_stmt,
									    depth + 1, ret);

						SqlColumnAlias *new_column_alias = NULL;
						SqlStatement *	tmp_stmt = cur_cla->column_list->v.column_list->value;
						column_table_alias = NULL;
						if (value_STATEMENT == tmp_stmt->type) {
							SqlValue *inner_value;
							UNPACK_SQL_STATEMENT(inner_value, tmp_stmt, value);
							if (COERCE_TYPE == inner_value->type) {
								if (column_alias_STATEMENT == inner_value->v.coerce_target->type) {
									new_column_alias
									    = inner_value->v.coerce_target->v.column_alias;
									UNPACK_SQL_STATEMENT(column_table_alias,
											     new_column_alias->table_alias_stmt,
											     table_alias);
								}
							}
						} else if (tmp_stmt->type == column_alias_STATEMENT) {
							new_column_alias
							    = cur_cla->column_list->v.column_list->value->v.column_alias;
							UNPACK_SQL_STATEMENT(column_table_alias, new_column_alias->table_alias_stmt,
									     table_alias);
						} else if (table_alias_STATEMENT == tmp_stmt->type) {
							UNPACK_SQL_STATEMENT(column_table_alias, tmp_stmt, table_alias);
						}
						if (column_table_alias) {
							parent_table_alias = column_table_alias->parent_table_alias;
							if (parent_table_alias == table_alias) {
								if (0 < parent_table_alias->aggregate_depth) {
									parent_table_alias
									    ->aggregate_function_or_group_by_or_having_specified
									    |= GROUP_BY_SPECIFIED;
								} else if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE
									   == parent_table_alias->aggregate_depth) {
									SqlValue *value;
									if ((NULL != new_column_alias)
									    && ((NULL != new_column_alias->column)
										&& (value_STATEMENT
										    == new_column_alias->column->type))) {
										UNPACK_SQL_STATEMENT(
										    value, new_column_alias->column, value);
									} else {
										value = NULL;
									}
									/* `group_by_column_count` and `group_by_column_number` in
									 * case of TABLE_ASTERISK is updated by
									 * `process_table_asterisk_cla()` invocation so no need to
									 * do it here.
									 */
									if (tmp_stmt->type == table_alias_STATEMENT) {
										++parent_table_alias->group_by_column_count;
									} else if ((NULL == value)
										   || (TABLE_ASTERISK != value->type)) {
										new_column_alias->group_by_column_number
										    = ++parent_table_alias->group_by_column_count;
									}
								}
							}
						} else {
							/* Case of the column_list_alias node not being either a table_alias or
							 * column_alias We are sure that this node belongs to the present query So
							 * go ahead and update the current table_alias
							 */
							/* Ex: select 1+1 from names group by 1;
							 * an expression is replacing 1
							 * An expression can be constant or one with a column
							 * If column is involved then based on whether it belongs to
							 * inner query or not needs to be used to update
							 * `table_alias->group_by_column_count`
							 */
							if (0 < table_alias->aggregate_depth)
								table_alias->aggregate_function_or_group_by_or_having_specified
								    |= GROUP_BY_SPECIFIED;
							SqlStatement *tmp = cur_cla->column_list->v.column_list->value;
							if ((tmp->type == value_STATEMENT)
							    && (tmp->v.value->type != CALCULATED_VALUE))
								table_alias->group_by_column_count++;
							else {

								table_alias->group_by_column_count++;
							}
						}
					}
				} else {
					if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE != table_alias->aggregate_depth) {

						/* Case (3) : Case of ORDER BY column expression */
						SqlSelectStatement *select;
						SqlOptionalKeyword *keywords, *keyword;

						/* Check if SELECT DISTINCT was specified */
						UNPACK_SQL_STATEMENT(select, table_alias->table, select);
						UNPACK_SQL_STATEMENT(keywords, select->optional_words, keyword);
						keyword = get_keyword_from_keywords(keywords, OPTIONAL_DISTINCT);
						if (NULL != keyword) {
							/* SELECT DISTINCT was specified. Check if the ORDER BY column expression
							 * matches some column specification in the SELECT column list. If so that
							 * is good. If not issue an error (see YDBOcto#461 for details).
							 */
							if (!match_column_list_alias_in_select_column_list(cur_cla,
													   select->select_list)) {
								ERROR(ERR_ORDER_BY_SELECT_DISTINCT, "");
								yyerror(NULL, NULL, &cur_cla->column_list, NULL, NULL, NULL);
								result = 1;
								break;
							}
						}
					} else {
						assert(AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth);
						UNPACK_SQL_STATEMENT(cur_cl, cur_cla->column_list, column_list);
						/* 1. If this is a column reference `group_by_column_count` and
						 * `group_by_column_number` is set in COLUMN_REFERENCE case block under
						 * value_STATEMENT so don't update here.
						 * 2. If is not a column reference this is an expression, increment
						 * `group_by_column_count`.
						 * 3. In case of an expression we do not update its `group_by_column_num` because
						 * this value doesn't get propogated to the same expression in other clauses. We
						 * rely on expression matching logic which happens in the second iteration of
						 *    qualify_statement() calls from qualify_query() `for` loop to update this
						 * field.
						 */
						if (column_alias_STATEMENT != cur_cl->value->type) {
							table_alias->group_by_column_count++;
						}
					}
				}
			}
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);
		break;
	case create_table_STATEMENT:
	case select_STATEMENT:
	case table_value_STATEMENT:
	case insert_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
	case join_STATEMENT:
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
	case display_relation_STATEMENT:
	case invalid_STATEMENT:
		/* Do not add "default:" case as we want to enumerate each explicit case here instead of having a
		 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
		 */
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		assert(FALSE);
		result = 1;
		break;
	}
	if ((NULL != ret) && (NULL != ret->max_unique_id)) {
		/* Caller has requested us to store the maximum unique_id seen under this subtree. So do that. */
		stmt->hash_canonical_query_cycle = (uint64_t)(*ret->max_unique_id);
		*ret->max_unique_id = MAX(*ret->max_unique_id, save_max_unique_id);
	}
	return result;
}
