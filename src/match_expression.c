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

/* This function facilitates the expansion of references to both key and non-key columns specified in `EXTRACT` DDL expressions
 * into proper M source code. The function behaves as follows:
 *
 * Checks whether `start` marks the beginning of either a "keys(" expression or a "values(" expression,
 * and, in either case, extracts the column name into `column` and returns an ExpressionMatchType
 * signaling which of these expressions was matched on, i.e. `ValuesExpression` or `KeysExpression`.
 *
 * If either a "keys(" or "values(" expression is matched, but the value in parentheses, i.e. the column name,
 * exceeds `column_size`, then `MatchExpressionOFlow` is returned.
 *
 * If neither neither "keys(" or "values(" is found at the beginning of `start`, then `NoMatchExpression`
 * is returned to the caller.
 */
ExpressionMatchType match_expression(char *start, char *column, int *expr_len, int column_size, char prev) {
	char		   *c, *c2, *column_start;
	char		   *keys = "keys(";
	char		   *values = "values(";
	int		    paren_count = 0;
	ExpressionMatchType match;

	assert((OCTO_MAX_IDENT + 1) == column_size);
	*expr_len = 0;
	c = start;
	match = KeysExpression;
	for (c2 = keys; ('\0' != *c) && ('\0' != *c2); c2++, c++) {
		if (*c != *c2) {
			match = NoMatchExpression;
			break;
		}
	}
	if (NoMatchExpression == match) {
		c = start;
		match = ValuesExpression;
		for (c2 = values; ('\0' != *c) && ('\0' != *c2); c2++, c++) {
			if (*c != *c2) {
				match = NoMatchExpression;
				break;
			}
		}
	}
	if ((NoMatchExpression == match) || (('(' != prev) && (',' != prev))) {
		/* All `keys()` and `values()` expressions appear in place of keys on M variables. Accordingly, all such expressions
		 * must be preceded by either '(' (first key in GVN) or ',' (subsequent key in GVN). So, if neither of these
		 * characters appears first in `start`, then we know immediately that no match is possible and set `match`
		 * accordingly.
		 */
		return NoMatchExpression;
	}

	paren_count = 1;
	column_start = c;

	while (paren_count && ('\0' != *c)) {
		switch (*c) {
		case '(':
			paren_count++;
			break;
		case ')':
			paren_count--;
			break;
		default:
			break;
		}
		c++;
	}
	if (0 == paren_count) {
		char *end;

		end = c;
		c--; /* Go back one byte as c will include the right paren which we don't want in "column" */
		assert(')' == *c);
		/* When a valid column name is specified inside keys(), "c" will include the trailing double quote
		 * which we don't want in "column". So go back 1 byte. Note that in case an invalid column name
		 * is specified inside keys(), for example [keys(abcd)], we won't have the trailing or the leading
		 * double quote. Hence the if checks below.
		 */
		if ('"' == *(c - 1)) {
			c--;
		}
		if ('"' == *column_start) {
			/* Go 1 byte past "column_start" as it will include the double quote which we don't want in "column". */
			column_start++;
		}
		if ((column_size < (c - column_start)) || (c < column_start)) {
			return MatchExpressionOFlow;
		}

		char *d = column;
		char *c2 = column_start;
		memcpy(d, c2, c - c2);
		d[c - c2] = '\0';
		*expr_len = end - start;
		return match;
	}
	return NoMatchExpression;
}
