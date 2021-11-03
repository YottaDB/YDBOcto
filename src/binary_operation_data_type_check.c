/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
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
			if (!result) {
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
	default:
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
