/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
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

/* This function does type checking of the input binary operation "binary" and issues errors as appropriate.
 * Additional input parameters "child_type[0]" and "child_type[1]" hold the type of the 2 binary operands.
 * Output parameter "type" holds the type of the result of the binary operation at function return.
 */
int binary_operation_data_type_check(SqlBinaryOperation *binary, SqlValueType child_type[2], SqlValueType *type,
				     ParseContext *parse_context) {
	/* CAST_AMBIGUOUS_TYPES() call below casts NUL_VALUE type to TABLE_ASTERISK type if one of the operands
	 * is of type TABLE_ASTERISK. Save the original type in such cases as some operations between TABLE_ASTERISK and NULL are
	 * valid and we want to ensure no error is issued for such usages.
	 */
	SqlValueType orig_child_type[2];
	orig_child_type[0] = child_type[0];
	orig_child_type[1] = child_type[1];

	int result;
	result = 0;
	CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result, parse_context);
	switch (binary->operation) {
	case ADDITION:
	case SUBTRACTION:
	case DIVISION:
	case MULTIPLICATION:
	case MODULO:;
		int i;

		for (i = 0; i < 2; i++) {
			switch (child_type[i]) {
			case INTEGER_LITERAL:
			case NUMERIC_LITERAL:
			case NUL_VALUE:
				/* These types are acceptable for arithmetic operations */
				break;
			default:
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[i], "arithmetic operations", &binary->operands[i],
							       result);
			}
		}
		*type = child_type[0];
		break;
	case CONCAT:
		/* Postgres allows || operator as long as at least one operand is STRING type or both operands are NULLs.
		 * Otherwise it issues an error. Do the same in Octo for compatibility.
		 */
		if (!IS_STRING_TYPE(child_type[0]) && !IS_STRING_TYPE(child_type[1])) {
			if (((TABLE_ASTERISK == child_type[0]) && (TABLE_ASTERISK == child_type[1]))
			    && ((NUL_VALUE == orig_child_type[0]) || (NUL_VALUE == orig_child_type[1]))) {
				// Concatanation between NUL_VALUE and TABLE_ASTERISK is valid, allow this usage
			} else if (!result) {
				int i;

				for (i = 0; i < 2; i++) {
					ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[i], "|| operator", &binary->operands[i], result);
				}
			}
		} else {
			SqlStatement **target;
			SqlValueType   tmp_type;

			/* If one operand is BOOLEAN_VALUE, add type cast operator (::string) to it.
			 * Not needed for INTEGER_LITERAL or NUMERIC_LITERAL as M handles this fine.
			 */
			if (BOOLEAN_VALUE == child_type[0]) {
				assert(BOOLEAN_VALUE != child_type[1]);
				tmp_type = child_type[0];
				target = &binary->operands[0];
			} else if (BOOLEAN_VALUE == child_type[1]) {
				assert(BOOLEAN_VALUE != child_type[0]);
				tmp_type = child_type[1];
				target = &binary->operands[1];
			} else {
				target = NULL;
			}
			if (NULL != target) {
				SqlStatement *sql_stmt;
				SqlValue *    value;

				SQL_STATEMENT(sql_stmt, value_STATEMENT);
				MALLOC_STATEMENT(sql_stmt, value, SqlValue);
				UNPACK_SQL_STATEMENT(value, sql_stmt, value);
				value->type = COERCE_TYPE;
				value->coerced_type.data_type = STRING_TYPE;
				value->coerced_type.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;
				value->coerced_type.scale = SCALE_UNSPECIFIED;
				value->coerced_type.size_or_precision_parameter_index = 0;
				value->coerced_type.scale_parameter_index = 0;
				value->pre_coerced_type = tmp_type;
				value->v.coerce_target = *target;
				*target = sql_stmt;
			}
		}
		child_type[0] = child_type[1] = *type = STRING_LITERAL;
		break;
	case BOOLEAN_OR:
	case BOOLEAN_AND:
		if (!result) {
			int i;

			for (i = 0; i < 2; i++) {
				if (!IS_BOOLEAN_TYPE(child_type[i])) {
					ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[i], "boolean operations", &binary->operands[i],
								       result);
				}
			}
		}
		*type = BOOLEAN_VALUE;
		break;
	case BOOLEAN_IS:
	case BOOLEAN_IS_NOT:
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
		// Comparison operation, if types are TABLE_ASTERISK make sure their tables are compatible
		if (!result && ((TABLE_ASTERISK == child_type[0]) && (TABLE_ASTERISK == child_type[1]))) {
			result = validate_table_asterisk_binary_operation(binary, orig_child_type, parse_context);
			if (result) {
				break;
			}
		} else if (!result) {
			if ((BOOLEAN_IN == binary->operation) || (BOOLEAN_NOT_IN == binary->operation)) {
				if (TABLE_ASTERISK == child_type[0]) {
					/* TABLE_ASTERISK IN TABLE_ASTERISK is taken care above.
					 * This is a TABLE_ASTERISK IN non_TABLE_ASTERISK comparison.
					 * The types will missmatch, `break` as code after this will issue the error.
					 */
					break;
				}
				// Check if the left operand is a reference and if its a set_operation
				if ((column_alias_STATEMENT == binary->operands[0]->type)
				    && (NULL != binary->operands[0]->v.column_alias->set_oper_stmt)) {
					child_type[0] = get_set_operation_column_alias_type(binary->operands[0]);
				}
				// Look at each item in the list and break when there is a missmatch
				if (column_list_STATEMENT == binary->operands[1]->type) {
					SqlColumnList *in_cl_list, *in_cl_list_cur;
					UNPACK_SQL_STATEMENT(in_cl_list, binary->operands[1], column_list);
					in_cl_list_cur = in_cl_list;
					do {
						SqlStatement *right_stmt = in_cl_list_cur->value;
						if ((column_alias_STATEMENT == right_stmt->type)
						    && (NULL != right_stmt->v.column_alias->set_oper_stmt)) {
							child_type[1] = get_set_operation_column_alias_type(right_stmt);
							if (child_type[0] != child_type[1]) {
								/* Type missmatch.
								 * Cast the types and let code at the end take care of error issue
								 * if any.
								 */
								CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result,
										     parse_context);
								break;
							}
						}
						in_cl_list_cur = in_cl_list_cur->next;
					} while (in_cl_list_cur != in_cl_list);
				}
			} else if (((column_alias_STATEMENT == binary->operands[0]->type)
				    && (column_alias_STATEMENT == binary->operands[1]->type))
				   || ((column_alias_STATEMENT == binary->operands[0]->type)
				       || (column_alias_STATEMENT == binary->operands[1]->type))) {
				// Either both operand is a column alias or one of them is. Update child_type for the column alias.
				int index, count;
				if ((column_alias_STATEMENT == binary->operands[0]->type)
				    && (column_alias_STATEMENT == binary->operands[1]->type)) {
					boolean_t left_is_table_asterisk, right_is_table_asterisk;
					left_is_table_asterisk = (TABLE_ASTERISK == child_type[0]) ? TRUE : FALSE;
					right_is_table_asterisk = (TABLE_ASTERISK == child_type[1]) ? TRUE : FALSE;
					/* The first if block in this chain handles comparison between two `table.*` values, do not
					 * expect such usage to reach this code block. Assert that both are not `table.*`.
					 */
					assert(!((TRUE == left_is_table_asterisk) && (TRUE == right_is_table_asterisk)));
					if (left_is_table_asterisk || right_is_table_asterisk) {
						/* This is a comparion between table.* and a regular column.
						 * Issue error as its an invalid usage.
						 */
						ERROR(ERR_TABLE_ASTERISK_SCALAR_COMPARISON, "");
						for (int i = 0; i < 2; i++) {
							yyerror(&binary->operands[i]->loc, NULL, NULL, NULL, NULL, NULL);
						}
						return 1;
					} else {
						index = 0;
						count = 2;
					}
				} else {
					if (column_alias_STATEMENT == binary->operands[0]->type) {
						index = 0;
						count = 1;
					} else {
						index = 1;
						count = 2;
					}
				}
				SqlColumnAlias *ca;
				for (int i = index; i < count; i++) {
					UNPACK_SQL_STATEMENT(ca, binary->operands[i], column_alias);
					if (NULL != ca->set_oper_stmt) {
						if (is_stmt_table_asterisk(ca->column)) {
							/* `table.*`, in this case we want the TABLE_ASTERISK as the type which is
							 * already set so nothing to do.
							 */
							continue;
						} else {
							child_type[i] = get_set_operation_column_alias_type(binary->operands[i]);
						}
					}
				}
				// Cast the new types set and let code at the end take care of error issue if any
				CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result, parse_context);
			}
		}
		*type = BOOLEAN_VALUE;
		break;
	}
	if (!result && (child_type[0] != child_type[1])) {
		int i;

		ERROR(ERR_TYPE_MISMATCH, get_user_visible_type_string(child_type[0]), get_user_visible_type_string(child_type[1]));
		for (i = 0; i < 2; i++) {
			yyerror(&binary->operands[i]->loc, NULL, NULL, NULL, NULL, NULL);
		}
		result = 1;
	}
	return result;
}
