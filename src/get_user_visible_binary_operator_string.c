/****************************************************************
 *								*
 * Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Returns a string corresponding to the binary operator (input parameter "operation") */
char *get_user_visible_binary_operator_string(enum BinaryOperations operation) {
	char *ret;

	switch (operation) {
	case ADDITION:
		ret = "+";
		break;
	case SUBTRACTION:
		ret = "-";
		break;
	case DIVISION:
		ret = "/";
		break;
	case MULTIPLICATION:
		ret = "*";
		break;
	case MODULO:
		ret = "%";
		break;
	case CONCAT:
		ret = "||";
		break;
	case BOOLEAN_OR:
		ret = "OR";
		break;
	case BOOLEAN_AND:
		ret = "AND";
		break;
	case BOOLEAN_IS:
		ret = "IS";
		break;
	case BOOLEAN_IS_NOT:
		ret = "IS NOT";
		break;
	case BOOLEAN_EQUALS:
		ret = "=";
		break;
	case BOOLEAN_NOT_EQUALS:
		ret = "!=";
		break;
	case BOOLEAN_LESS_THAN:
		ret = "<";
		break;
	case BOOLEAN_GREATER_THAN:
		ret = ">";
		break;
	case BOOLEAN_LESS_THAN_OR_EQUALS:
		ret = "<=";
		break;
	case BOOLEAN_GREATER_THAN_OR_EQUALS:
		ret = ">=";
		break;
	case BOOLEAN_REGEX_SENSITIVE:
		ret = "~";
		break;
	case BOOLEAN_REGEX_INSENSITIVE:
		ret = "~*";
		break;
	case BOOLEAN_REGEX_SENSITIVE_LIKE:
		ret = "LIKE";
		break;
	case BOOLEAN_REGEX_INSENSITIVE_LIKE:
		ret = "ILIKE";
		break;
	case BOOLEAN_REGEX_SENSITIVE_SIMILARTO:
		ret = "SIMILAR TO";
		break;
	case BOOLEAN_REGEX_INSENSITIVE_SIMILARTO:
		assert(FALSE); /* A case-insensitive SIMILAR TO is impossible */
		ret = "";
		break;
	case BOOLEAN_IN:
		ret = "IN";
		break;
	case BOOLEAN_NOT_IN:
		ret = "NOT IN";
		break;
	case BOOLEAN_ANY_EQUALS:
		ret = "= ANY";
		break;
	case BOOLEAN_ANY_NOT_EQUALS:
		ret = "!= ANY";
		break;
	case BOOLEAN_ANY_LESS_THAN:
		ret = "< ANY";
		break;
	case BOOLEAN_ANY_GREATER_THAN:
		ret = "> ANY";
		break;
	case BOOLEAN_ANY_LESS_THAN_OR_EQUALS:
		ret = "<= ANY";
		break;
	case BOOLEAN_ANY_GREATER_THAN_OR_EQUALS:
		ret = ">= ANY";
		break;
	case BOOLEAN_ALL_EQUALS:
		ret = "= ALL";
		break;
	case BOOLEAN_ALL_NOT_EQUALS:
		ret = "!= ALL";
		break;
	case BOOLEAN_ALL_LESS_THAN:
		ret = "< ALL";
		break;
	case BOOLEAN_ALL_GREATER_THAN:
		ret = "> ALL";
		break;
	case BOOLEAN_ALL_LESS_THAN_OR_EQUALS:
		ret = "<= ALL";
		break;
	case BOOLEAN_ALL_GREATER_THAN_OR_EQUALS:
		ret = ">= ALL";
		break;
	case DATE_TIME_ADDITION:
		ret = "+";
		break;
	case DATE_TIME_SUBTRACTION:
		ret = "-";
		break;
	default:
		/* All valid code paths are already enumerated above. So this should be an impossible code path.
		 * But is there to avoid false [-Wmaybe-uninitialized] warnings from compiler.
		 */
		assert(FALSE);
		ret = "";
		break;
	}
	return ret;
}
