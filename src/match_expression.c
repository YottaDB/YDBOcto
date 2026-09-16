/****************************************************************
 *								*
 * Copyright (c) 2022-2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>
#include <ctype.h>

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
 * If neither "keys(" nor "values(" is found at the beginning of `start`, then `NoMatchExpression`
 * is returned to the caller.
 *
 * `prev` is the character that immediately precedes `start` in the M expression being scanned; it decides whether a
 * "keys(" or "values(" at `start` is a column reference at all (see the comment on the `switch (prev)` below).
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
	if (NoMatchExpression == match) {
		return NoMatchExpression;
	}

	/* At this point `start` begins with the bytes "keys(" or "values(". That by itself does not make it a column
	 * reference. It is one only if the matched text stands on its own, i.e. it is neither the tail end of a longer
	 * M name (e.g. "mykeys(" or "x2values(") nor the start of a name that lives in some other namespace (e.g. the
	 * global "^keys(", the extrinsic "$$values(") nor the interior of an M string literal. What tells these apart is
	 * `prev`, the character that immediately precedes `start` in the M expression. Callers that begin the scan at the
	 * first byte of the expression have no such character and pass '(' to signal that the match stands on its own.
	 *
	 * Anything else can legitimately precede a column reference. EXTRACT and GLOBAL specifications hold arbitrary M
	 * expressions, so a column reference can follow any M operator: ':' in a $SELECT, '_' concatenation, relational
	 * and arithmetic operators, '\'' negation etc., in addition to the '(' and ',' that precede a subscript. Hence
	 * `prev` is checked below against the characters that rule a match out instead of the characters that allow one.
	 */
	switch (prev) {
	case '"': /* "keys(" lies inside an M string literal */
	case '^': /* global variable name, e.g. ^keys("a") */
	case '$': /* intrinsic function or extrinsic call, e.g. $values(...) or $$keys^rtn(...) */
	case '&': /* external call, e.g. $&keys(1) */
	case '%': /* M name that starts with '%', e.g. %keys(1) */
	case '.': /* actual passed by reference, e.g. .values(1) */
		return NoMatchExpression;
	default:
		if (isalnum((int)(unsigned char)prev)) {
			/* "keys(" is the tail end of a longer M name, e.g. "mykeys(" */
			return NoMatchExpression;
		}
		break;
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
