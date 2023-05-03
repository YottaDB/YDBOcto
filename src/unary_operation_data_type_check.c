/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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

#define ISSUE_ERROR(OPERAND, MSG, STR, RESULT)                    \
	{                                                         \
		ERROR((MSG), (STR));                              \
		yyerror(NULL, NULL, (OPERAND), NULL, NULL, NULL); \
		RESULT = 1;                                       \
	}

/* This function does type checking of the input unary operation "unary" and issues errors as appropriate.
 * Additional input parameter "child_type[0]" holds the type of the unary operand.
 * Output parameter "type" holds the type of the result of the unary operation at function return.
 */
int unary_operation_data_type_check(SqlUnaryOperation *unary, SqlValueType child_type[2], SqlValueType *type) {
	int result;
	result = 0;
	/* Check for type mismatches */
	switch (unary->operation) {
	case FORCE_NUM:
	case NEGATIVE:
		switch (child_type[0]) {
		case TABLE_ASTERISK:
			ISSUE_TYPE_COMPATIBILITY_ERROR(child_type[0], "+ or - operation", &unary->operand, result);
			break;
		case BOOLEAN_OR_STRING_LITERAL:
			FIX_TYPE_TO_STRING_LITERAL(child_type[0]);
			/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
			/* fall through */
		case STRING_LITERAL:
		case COLUMN_REFERENCE:
		case CALCULATED_VALUE:
		case FUNCTION_NAME:
		case FUNCTION_HASH:
		case PARAMETER_VALUE:
		case NUL_VALUE:
		case COERCE_TYPE:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
		case BOOLEAN_VALUE:
			/* Unary + and - operators cannot be used on non-numeric or non-integer types */
			ISSUE_ERROR(&unary->operand, ERR_INVALID_INPUT_SYNTAX, get_user_visible_type_string(child_type[0]), result);
			break;
		case INTEGER_LITERAL:
		case NUMERIC_LITERAL:
			*type = child_type[0];
			break;
		case SELECT_ASTERISK:
			assert(FALSE);
			break;
		}
		break;
	case BOOLEAN_NOT:
		switch (child_type[0]) {
		case STRING_LITERAL:
			if ((value_STATEMENT == unary->operand->type) && (STRING_LITERAL == unary->operand->v.value->type)) {
				ISSUE_ERROR(&unary->operand, ERR_INVALID_BOOLEAN_SYNTAX,
					    get_user_visible_type_string(child_type[0]), result);
			} else {
				ISSUE_ERROR(&unary->operand, ERR_NOT_OPERATION_TYPE_MISMATCH,
					    get_user_visible_type_string(child_type[0]), result);
			}
			break;
		case TABLE_ASTERISK:
		case COLUMN_REFERENCE:
		case CALCULATED_VALUE:
		case FUNCTION_NAME:
		case FUNCTION_HASH:
		case PARAMETER_VALUE:
		case COERCE_TYPE:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case INTEGER_LITERAL:
		case NUMERIC_LITERAL:
		case UNKNOWN_SqlValueType:
			// not a boolean or null value
			ISSUE_ERROR(&unary->operand, ERR_NOT_OPERATION_TYPE_MISMATCH, get_user_visible_type_string(child_type[0]),
				    result);
			break;
		case BOOLEAN_OR_STRING_LITERAL:
			FIX_TYPE_TO_BOOLEAN_VALUE(child_type[0]);
			/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
			/* fall through */
		case BOOLEAN_VALUE:
		case NUL_VALUE:
			*type = child_type[0];
			break;
		case SELECT_ASTERISK:
			assert(FALSE);
			break;
		}
		break;
	case BOOLEAN_NOT_EXISTS:
		assert(FALSE);
		break;
	case BOOLEAN_EXISTS:
		/* If the unary operation is EXISTS, set the type of the result to BOOLEAN, not the type inherited from the
		 * sub-query passed to EXISTS. */
		*type = BOOLEAN_VALUE;
		break;
	}
	return result;
}
