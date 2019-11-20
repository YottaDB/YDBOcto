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

int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *column_list_alias,
								int depth, SqlColumnListAlias **ret_cla) {
	SqlUnaryOperation	*unary;
	SqlBinaryOperation	*binary;
	SqlFunctionCall		*fc;
	SqlColumnList		*start_cl, *cur_cl;
	SqlValue		*value;
	SqlCaseStatement	*cas;
	SqlCaseBranchStatement	*cas_branch, *cur_branch;
	SqlColumnListAlias	*start_cla, *cur_cla;
	int			result;
	SqlColumnAlias		*new_column_alias;

	result = 0;
	if (NULL == stmt)
		return result;
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
			new_column_alias = qualify_column_name(value, tables, column_list_alias, depth + 1, ret_cla);
			result = ((NULL == new_column_alias) && ((NULL == ret_cla) || (NULL == *ret_cla)));
			if (result) {
				print_yyloc(&stmt->loc);
			} else {
				if ((NULL == ret_cla) || (NULL == *ret_cla)) {
					// Convert this statement to a qualified one by changing the "type".
					// Note: "qualify_column_name.c" relies on the below for qualifying column names
					stmt->type = column_alias_STATEMENT;
					stmt->v.column_alias = new_column_alias;
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
			result |= qualify_statement(value->v.calculated, tables, column_list_alias, depth + 1, ret_cla);
			break;
		case FUNCTION_NAME:
			// If it starts with '$$', trim those off and leave it alone (MUMPS expression)
			// Else, match it with a value from the dictionary in ^octo("functions")
			result = qualify_function_name(stmt);
			if (result) {
				print_yyloc(&stmt->loc);
			}
			break;
		case COERCE_TYPE:
			result |= qualify_statement(value->v.coerce_target, tables, column_list_alias, depth + 1, ret_cla);
			if (result) {
				print_yyloc(&stmt->loc);
			}
			break;
		default:
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		result |= qualify_statement(binary->operands[0], tables, column_list_alias, depth + 1, ret_cla);
		result |= qualify_statement(binary->operands[1], tables, column_list_alias, depth + 1, ret_cla);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		result |= qualify_statement(unary->operand, tables, column_list_alias, depth + 1, ret_cla);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		result |= qualify_statement(fc->function_name, tables, column_list_alias, depth + 1, ret_cla);
		result |= qualify_statement(fc->parameters, tables, column_list_alias, depth + 1, ret_cla);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		result |= qualify_statement(cas->value, tables, column_list_alias, depth + 1, ret_cla);
		result |= qualify_statement(cas->branches, tables, column_list_alias, depth + 1, ret_cla);
		result |= qualify_statement(cas->optional_else, tables, column_list_alias, depth + 1, ret_cla);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		do {
			result |= qualify_statement(cur_branch->condition, tables, column_list_alias, depth + 1, ret_cla);
			result |= qualify_statement(cur_branch->value, tables, column_list_alias, depth + 1, ret_cla);
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case column_list_STATEMENT:
		// This is a result of a value-list
		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			result |= qualify_statement(cur_cl->value, tables, column_list_alias, depth + 1, ret_cla);
			cur_cl = cur_cl->next;
		} while (cur_cl != start_cl);
		break;
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		result |= qualify_query(stmt, tables);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		cur_cla = start_cla;
		do {
			assert(depth || (NULL == ret_cla) || (NULL == *ret_cla));/* assert that caller has initialized "*ret_cla" */
			result |= qualify_statement(cur_cla->column_list, tables, column_list_alias, depth + 1, ret_cla);
			if ((NULL != ret_cla) && (0 == depth)) {
				SqlColumnListAlias	*qualified_cla;
				int			column_number;
				char			*str;
				boolean_t		order_by_alias;

				/* "ret_cla" is non-NULL implies this call is for an ORDER BY column list.
				 * "depth" == 0 implies "cur_cla" is one of the ORDER BY columns
				 * (i.e. not a cla corresponding to an inner evaluation in the ORDER BY expression).
				 * There are 2 cases to handle.
				 */
				if (NULL != *ret_cla) {
					/* Case (1) : If "*ret_cla" is non-NULL, it is a case of ORDER BY using an alias name */

					order_by_alias = TRUE;
					/* "QUALIFY_COLUMN_REFERENCE" : This code block finishes the column name validation
					 * that was not finished in the "qualify_column_name" call done in the
					 * "case COLUMN_REFERENCE:" code block above. We found another existing cla that is
					 * already qualified and matches the current non-qualified cla. So fix the current
					 * cla to mirror the already qualified cla.
					 */
					qualified_cla = *ret_cla;
					*ret_cla = NULL;	/* initialize for next call to "qualify_statement" */
				} else {
					/* Case (2) : If "*ret_cla" is NULL, check if this is a case of ORDER BY column-number.
					 * If so, point "cur_cla" to corresponding cla from the SELECT column list.
					 */
					SqlColumnList	*col_list;
					boolean_t	error_encountered = FALSE;
					boolean_t	is_positive_numeric_literal, is_negative_numeric_literal;

					order_by_alias = FALSE;
					UNPACK_SQL_STATEMENT(col_list, cur_cla->column_list, column_list);
					/* Check for positive numeric literal */
					is_positive_numeric_literal = ((value_STATEMENT == col_list->value->type)
									&& (NUMERIC_LITERAL == col_list->value->v.value->type));
					/* Check for negative numeric literal next */
					is_negative_numeric_literal = FALSE;
					if (!is_positive_numeric_literal) {
						if (unary_STATEMENT == col_list->value->type) {
							SqlUnaryOperation	*unary;

							UNPACK_SQL_STATEMENT(unary, col_list->value, unary);
							is_negative_numeric_literal =
								((NEGATIVE == unary->operation)
									&& (value_STATEMENT == unary->operand->type)
									&& (NUMERIC_LITERAL == unary->operand->v.value->type));
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
						char		*ptr, *ptr_top;
						long int	ret;

						for (ptr = str, ptr_top = str + strlen(str); ptr < ptr_top; ptr++) {
							if ('.' == *ptr) {
								ERROR(ERR_ORDER_BY_POSITION_NOT_INTEGER,
									is_negative_numeric_literal ? "-" : "", str);
								print_yyloc(&cur_cla->column_list->loc);
								error_encountered = 1;
								break;
							}
						}
						if (!error_encountered) {
							/* Now that we have confirmed the string only consists of the digits [0-9],
							 * check if it is a valid number that can be represented in an integer.
							 * If not issue error.
							 */
							ret = strtol(str, NULL, 10);
							if ((LONG_MIN == ret) || (LONG_MAX == ret)) {
								ERROR(ERR_ORDER_BY_POSITION_NOT_INTEGER,
									is_negative_numeric_literal ? "-" : "", str);
								print_yyloc(&cur_cla->column_list->loc);
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
								column_number = (int)ret;
								qualified_cla = get_column_list_alias_n_from_table_alias(
												column_list_alias, column_number);
							} else {
								qualified_cla = NULL;
							}
							if (NULL == qualified_cla) {
								ERROR(ERR_ORDER_BY_POSITION_INVALID,
									is_negative_numeric_literal ? "-" : "", str);
								print_yyloc(&cur_cla->column_list->loc);
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
					SqlTableAlias		*table_alias;

					cur_cla->column_list = qualified_cla->column_list;
					assert(NULL == cur_cla->alias);
					/* Note: It is not necessary to not copy " qualified_cla->alias" into "cur_cla->alias" */
					/* "cur_cla->keywords" might have ASC or DESC keywords (for ORDER BY)
					 * which the qualified_cla would not have so do not overwrite those.
					 */
					cur_cla->type = qualified_cla->type;
					UNPACK_SQL_STATEMENT(table_alias, column_list_alias, table_alias);
					/* Set unique_id and column_number fields. Assert that they are non-zero as
					 * "hash_canonical_query" skips hashing these fields if they are 0.
					 */
					cur_cla->tbl_and_col_id.unique_id = table_alias->unique_id;
					assert(cur_cla->tbl_and_col_id.unique_id);
					if (order_by_alias) {
						/* Find the column number from the matched cla */
						cur_cla->tbl_and_col_id.column_number =
							get_column_number_from_column_list_alias(qualified_cla, column_list_alias);
					} else {
						/* The column number was already specified. So use that as is. */
						cur_cla->tbl_and_col_id.column_number = column_number;
					}
					assert(cur_cla->tbl_and_col_id.column_number);
				}
			}
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);
		break;
	case table_STATEMENT:
		break;
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		result = 1;
		break;
	}
	return result;
}
