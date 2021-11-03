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

/* This function does type checking of the input unary operation "unary" and issues errors as appropriate.
 * Additional input parameter "child_type[0]" holds the type of the unary operand.
 * Output parameter "type" holds the type of the result of the unary operation at function return.
 */
int unary_operation_data_type_check(SqlUnaryOperation *unary, SqlValueType child_type[2], SqlValueType *type) {

	int result;

	result = 0;
	/* Check for type mismatches */
	if (((FORCE_NUM == unary->operation) || (NEGATIVE == unary->operation))
	    && ((INTEGER_LITERAL != child_type[0]) && (NUMERIC_LITERAL != child_type[0]))) {
		/* Unary + and - operators cannot be used on non-numeric or non-integer types */
		ERROR(ERR_INVALID_INPUT_SYNTAX, get_user_visible_type_string(child_type[0]));
		yyerror(NULL, NULL, &unary->operand, NULL, NULL, NULL);
		result = 1;
	} else {
		/* If the unary operation is EXISTS or IS NULL, then set the type of the result to BOOLEAN,
		 * not to the type inherited from the sub-query passed to EXISTS.
		 */
		assert(BOOLEAN_NOT_EXISTS != unary->operation);
		switch (unary->operation) {
		case BOOLEAN_EXISTS:
		case BOOLEAN_IS_NULL:
		case BOOLEAN_IS_NOT_NULL:
			*type = BOOLEAN_VALUE;
			break;
		default:
			*type = child_type[0];
			break;
		}
		assert((BOOLEAN_NOT != unary->operation) || (NUL_VALUE == *type) || IS_LITERAL_PARAMETER(*type));
	}
	return result;
}
