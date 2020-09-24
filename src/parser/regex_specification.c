/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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
			int is_not, ParseContext *parse_context) {
	SqlStatement *regex;

	CREATE_BINARY_STATEMENT(regex);
	if (is_sensitive) {
		if (REGEX_LIKE == regex_type) {
			/* If the pattern string has no special meaning characters, then case sensitive LIKE is the same as
			 * the EQUALS operator since LIKE matches the entire string and this is a case-sensitive match.
			 * The EQUALS operator has better chances of being optimized so use that instead of LIKE if possible.
			 */
			int status;

			status = regex_has_no_special_characters(op1, regex_type, parse_context);
			if (-1 == status) {
				/* Error : Forward error to caller so it can do YYABORT. */
				return 1;
			}
			assert((0 == status) || (1 == status));
			regex->v.binary->operation = ((0 == status) ? BOOLEAN_REGEX_SENSITIVE_LIKE : BOOLEAN_EQUALS);
		} else if (REGEX_SIMILARTO == regex_type) {
			/* If the pattern string has no special meaning characters, then LIKE is same as EQUALS operator.
			 * The latter has better chances of being optimized so check for that here.
			 */
			int status;

			status = regex_has_no_special_characters(op1, regex_type, parse_context);
			if (-1 == status) {
				/* Error : Forward error to caller so it can do YYABORT. */
				return 1;
			}
			assert((0 == status) || (1 == status));
			regex->v.binary->operation = ((0 == status) ? BOOLEAN_REGEX_SENSITIVE_SIMILARTO : BOOLEAN_EQUALS);
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
		if (REGEX_LIKE == regex_type) {
			regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE_LIKE;
		} else if (REGEX_SIMILARTO == regex_type) {
			regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE_SIMILARTO;
		} else {
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
