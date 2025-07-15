/****************************************************************
 *								*
 * Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_type_check.h"

/* The following macro is used by validate_table_asterisk_binary_operation() to check if any of the operands are
 * of type subquery and issue an error. Note we can reach here only if the type of the subquery is NUL_VALUE.
 * This macro is called for processing BOOLEAN_IN/BOOLEAN_NOT_IN or =,!=,<,>,>=,<= operation. We are sure that
 * the FIRST_OPERAND is `table.*` in the former case but we are not sure of this for the latter case, hence the
 * if check to determine which operand is a `table.*`.
 */
#define CHECK_AND_ISSUE_TABLE_ASTERISK_NULL_SUBQUERY_INCOMPATIBILITY_ERROR(FIRST_OPERAND, SECOND_OPERAND, RESULT)         \
	{                                                                                                                 \
		SqlStatement **operand;                                                                                   \
		if (is_stmt_table_asterisk(FIRST_OPERAND)) {                                                              \
			operand = &SECOND_OPERAND;                                                                        \
		} else {                                                                                                  \
			operand = &FIRST_OPERAND;                                                                         \
		}                                                                                                         \
		if ((table_alias_STATEMENT == (*operand)->type) || (set_operation_STATEMENT == (*operand)->type)) {       \
			/* This is a subquery. The only usage which can lead us here is a subquery returning ata of type  \
			 * NUL_VALUE is used as `table.*` comparison operand. Comparison between a `table.*`              \
			 * and NULL is not allowed in such a case.                                                        \
			 */                                                                                               \
			ISSUE_TYPE_COMPATIBILITY_ERROR(TABLE_ASTERISK, "NULL type subquery comparison", operand, RESULT); \
		}                                                                                                         \
	}

/* Get the two table.* operands which will undergo binary operation and validate its equality using helper function.
 * `orig_child_type` is used to avoid validations for specific operations when one of the operand is of NUL_VALUE type.
 * Note:
 * 1. In case of IN operation even if `orig_child_type` has `TABLE_ASTERISK`, it is possible to get NULL values in
 *    the IN list as CAST_AMBIGUOUS_TYPES() could have been invoked before the call to binary_operation_data_type_check()
 *    and its valid to have NULL and `table.*` in the same list. The function handles it by ignoring this value as its
 *    valid to compare a table.* with NULL.
 * 2. Apart from the cases in this function it is valid for `CONCAT` operation to have a `table.*` and NUL_VALUE type
 *    operand. This case is handled in binary_operation_data_type_check() itself.
 * Returns
 * a) 0 for valid comparison
 * b) 1 for invalid comparison after error
 */
int validate_table_asterisk_binary_operation(SqlBinaryOperation *binary, SqlValueType orig_child_type[2],
					     ParseContext *parse_context) {
	SqlStatement *first_operand = binary->operands[0];
	SqlStatement *second_operand = binary->operands[1];
	int	      result = 0;
	switch (binary->operation) {
	case BOOLEAN_IN:
	case BOOLEAN_NOT_IN:
		CHECK_AND_ISSUE_TABLE_ASTERISK_NULL_SUBQUERY_INCOMPATIBILITY_ERROR(first_operand, second_operand, result);
		if (result) {
			break;
		}
		/* Validate only if the operand is not of type `NUL_VALUE` because the following code does column count
		 * and column type check on two tables, if `NUL_VALUE` is one of the operand then there is nothing there
		 * to compare. Note in case of IN operation on an in-list, even if `orig_child_type` is `TABLE_ASTERISK` its
		 * possible that the underlying value is actually a `NULL` (The reasoning of why this is as such is
		 * mentioned in the if block specific to `NUL_VALUE` type value_STATEMENT below).
		 */
		if (!IS_NUL_VALUE(orig_child_type[0]) && !IS_NUL_VALUE(orig_child_type[1])) {
			SqlColumnAlias *first_column_alias;
			UNPACK_SQL_STATEMENT(first_column_alias, first_operand, column_alias);
			assert(is_stmt_table_asterisk(first_operand));
			assert(column_list_STATEMENT == second_operand->type);
			SqlColumnList *start_column_list, *cur_column_list;
			UNPACK_SQL_STATEMENT(start_column_list, second_operand, column_list);
			cur_column_list = start_column_list;
			do {
				if (!is_stmt_table_asterisk(cur_column_list->value)) {
					/* This is a valid usage
					 * Example: `n1.* not in (NULL,n1.*)`
					 * Example: `n1.* not in (NULL % NULL,n1.*)`
					 * The reasons why other types are not possible here are
					 * 1) validate_table_asterisk_binary_operation() is only invoked for `table.*`
					 *    compatible types i.e. either NULL or `table.*`
					 * 2) In binary_STATEMENT case of populate_data_type(), we call
					 *    populate_data_type_column_list() on the second operand of BOOLEAN_IN and
					 *    BOOLEAN_NOT_IN operation with the call back `ensure_same_type`.
					 *    This ensures that the list returned will always be of the same type and
					 *    not have items like `(1,n1.*)`. If it has such dissimilar types an error
					 *    is issued by the call back.
					 */
				} else {
					SqlColumnAlias *second_column_alias;
					UNPACK_SQL_STATEMENT(second_column_alias, cur_column_list->value, column_alias);
					result = compare_column_count_and_column_type_of_tables(first_column_alias,
												second_column_alias, parse_context);
					if (result) {
						yyerror(&cur_column_list->value->loc, NULL, NULL, NULL, NULL, NULL);
						break;
					}
				}
				cur_column_list = cur_column_list->next;
			} while (cur_column_list != start_column_list);
		}
		break;
	case BOOLEAN_IS:
	case BOOLEAN_IS_NOT:
		/* This operation is allowed with `table.*` and we are sure that the second operand is of type `NUL_VALUE` */
		assert(IS_NUL_VALUE(orig_child_type[1]));
		break;
	case BOOLEAN_EQUALS:
	case BOOLEAN_NOT_EQUALS:
	case BOOLEAN_LESS_THAN:
	case BOOLEAN_GREATER_THAN:
	case BOOLEAN_LESS_THAN_OR_EQUALS:
	case BOOLEAN_GREATER_THAN_OR_EQUALS:
		assert((is_stmt_table_asterisk(first_operand) || IS_NUL_VALUE(orig_child_type[0]))
		       && (is_stmt_table_asterisk(second_operand) || IS_NUL_VALUE(orig_child_type[1])));
		CHECK_AND_ISSUE_TABLE_ASTERISK_NULL_SUBQUERY_INCOMPATIBILITY_ERROR(first_operand, second_operand, result);
		if (result) {
			break;
		}
		/* Validate only if the operand is not of type NUL_VALUE because the following code does column count
		 * and column type check on two tables, if NUL_VALUE is one of the operand then there is nothing there
		 * to compare. Note in this case we expect the underlying operand type to be same as `orig_child_type` unlike IN
		 * operation case above.
		 */
		if (!IS_NUL_VALUE(orig_child_type[0]) && !IS_NUL_VALUE(orig_child_type[1])) {
			SqlColumnAlias *first_column_alias;
			UNPACK_SQL_STATEMENT(first_column_alias, first_operand, column_alias);

			assert(is_stmt_table_asterisk(first_operand) && is_stmt_table_asterisk(second_operand));
			SqlColumnAlias *second_column_alias;
			UNPACK_SQL_STATEMENT(second_column_alias, second_operand, column_alias);

			result = compare_column_count_and_column_type_of_tables(first_column_alias, second_column_alias,
										parse_context);
			if (result) {
				yyerror(&second_operand->loc, NULL, NULL, NULL, NULL, NULL);
			}
		} else {
			int index;
			if (is_stmt_table_asterisk(first_operand)) {
				// First operand is `table.*` check the other operand by setting index to right operand
				index = 1;
			} else {
				// Second operand is `table.*` check the other operand by setting index to left operand
				index = 0;
			}
			if (column_alias_STATEMENT == binary->operands[index]->type) {
				// This is a column alias and its being compared with a `table.*`
				// Issue error as this is not allowed. Postgres treats this is as value_type comparison with record.
				ERROR(ERR_TABLE_ASTERISK_SCALAR_COMPARISON, "");
				for (int i = 0; i < 2; i++) {
					yyerror(&binary->operands[i]->loc, NULL, NULL, NULL, NULL, NULL);
				}
				result = 1;
			}
		}
		break;
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
		/* Queries like the following can reach this code. Issue error in such cases.
		 * `select n1.column1 from (VALUES('testa')) n1 where n1.* < ANY (select NULL);`
		 */
		CHECK_AND_ISSUE_TABLE_ASTERISK_NULL_SUBQUERY_INCOMPATIBILITY_ERROR(first_operand, second_operand, result);
		assert(result);
		break;
	case BOOLEAN_REGEX_SENSITIVE:
	case BOOLEAN_REGEX_INSENSITIVE:
	case BOOLEAN_REGEX_SENSITIVE_LIKE:
	case BOOLEAN_REGEX_INSENSITIVE_LIKE:
	case BOOLEAN_REGEX_SENSITIVE_SIMILARTO:
	case ADDITION:
	case SUBTRACTION:
	case DIVISION:
	case MULTIPLICATION:
	case MODULO:
	case CONCAT:
	case BOOLEAN_OR:
	case BOOLEAN_AND:
	case DATE_TIME_ADDITION:
	case DATE_TIME_SUBTRACTION:
		assert(FALSE);
		result = 1;
		break;
	}
	return result;
}
