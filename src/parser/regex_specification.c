/****************************************************************
 *								*
 * Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_types.h"

#define CREATE_BINARY_STATEMENT(STMT)                               \
	{                                                           \
		SQL_STATEMENT(STMT, binary_STATEMENT);              \
		MALLOC_STATEMENT(STMT, binary, SqlBinaryOperation); \
	}

/* Returns 0 on success. 1 on failure. */
int regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, enum RegexType regex_type, int is_sensitive,
			int is_not) {
	SqlStatement *regex;

	CREATE_BINARY_STATEMENT(regex);
	if (is_sensitive) {
		if ((REGEX_LIKE == regex_type) || (REGEX_SIMILARTO == regex_type)) {
			regex->v.binary->operation
			    = (REGEX_LIKE == regex_type) ? BOOLEAN_REGEX_SENSITIVE_LIKE : BOOLEAN_REGEX_SENSITIVE_SIMILARTO;
		} else {
			assert(REGEX_TILDE == regex_type);
			/* The regex match is not an entire string match (i.e. a substring match is also okay) so we cannot
			 * convert this operator to EQUALS like we did the previous cases.
			 */
			regex->v.binary->operation = BOOLEAN_REGEX_SENSITIVE;
		}
	} else {
		/* The below matches are all case insensitive matches. And so cannot be converted
		 * into an EQUALS operator like we did cases in the "if()" block above.
		 */
		assert(REGEX_SIMILARTO != regex_type);
		if (REGEX_LIKE == regex_type) {
			regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE_LIKE;
		} else {
			assert(REGEX_TILDE == regex_type);
			regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE;
		}
	}
	regex->v.binary->operands[0] = op0;
	regex->v.binary->operands[1] = op1;
	if (is_not) {
		/* Negation specified */
		if (BOOLEAN_EQUALS == regex->v.binary->operation) {
			/* Convert EQUALS to NOT EQUALS */
			regex->v.binary->operation = BOOLEAN_NOT_EQUALS;
		} else {
			SqlStatement *not_stmt;

			/* Else: Surround the REGEX statement with a BOOLEAN_NOT statement */
			SQL_STATEMENT(not_stmt, unary_STATEMENT);
			MALLOC_STATEMENT(not_stmt, unary, SqlUnaryOperation);
			(not_stmt)->v.unary->operation = BOOLEAN_NOT;
			(not_stmt)->v.unary->operand = regex;
			regex = not_stmt;
		}
	}
	*stmt = regex;
	return 0;
}
