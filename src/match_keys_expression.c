/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
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

/**
 * Returns n > 0 if "start" marks the beginning of a "keys(...)" expression
 *  Populates "column" with the name of the column in that case, and returns the length of the "keys(...)" expression
 *  that got matched with the column name.
 * Returns -1 if the column name inside the keys(..) expression won't fit in "column" (of size "column_size")
 * Returns 0 if "start" does not mark the beginning of a "keys(...)" expression.
 */
int match_keys_expression(char *start, char *column, int column_size) {
	char *c, *c2, *column_start;
	char *keys = "keys(";
	int   paren_count = 0;

	assert((OCTO_MAX_IDENT + 1) == column_size);
	// First match on keys( and we can start parsing table name
	c = start;
	for (c2 = keys; '\0' != *c && '\0' != *c2; c2++, c++) {
		if (*c != *c2) {
			return 0;
		}
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
		c--; /* Go back one byte as c will include the right paren which we don't want in "column" */
		assert(')' == *c);
		c--; /* Go back one byte as c will include the double quote which we don't want in "column" */
		assert('"' == *c);
		assert('"' == *column_start); /* Go 1 byte past "column_start" as it will include the double quote
					       * which we don't want in "column".
					       */
		column_start++;
		if ((column_size < (c - column_start)) || (c < column_start)) {
			return -1;
		}

		char *d = column;
		char *c2 = column_start;
		memcpy(d, c2, c - c2);
		d[c - c2] = '\0';
		assert('"' != column[0]);
		assert('"' == *c);
		assert(!(column[0] == '"' && column[1] == '"'));
		return c - start + 2;
	}
	return 0;
}
