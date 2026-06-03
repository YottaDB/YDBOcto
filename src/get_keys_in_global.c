/****************************************************************
 *								*
 * Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	*
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

/* Walks a GLOBAL mapping string and returns the "keys(...)" subscripts it references, in the order they
 * appear. For example, for the mapping ^LCF(keys("reg"),keys("req")) it returns 2 and fills "key_columns"
 * with the SqlColumn structures for "reg" and "req" (in that order).
 *
 * This is the number of primary-key columns that the global maps to. It can be fewer than the table's
 * full primary key when a column maps to a shallower (parent) node than the row, in which case the
 * leftover primary-key columns live on a deeper node. The referenced columns can also appear in a
 * different order than the table's primary key, since Octo does not require a column's GLOBAL mapping to
 * list keys in primary-key order (see "validate_global_keyword.c"). A bare, unsubscripted global (no
 * parentheses) has no "keys(...)" references and so returns 0.
 *
 * "key_columns" must point to a caller-allocated array of at least MAX_KEY_COUNT entries.
 *
 * The string is scanned one character at a time, using "match_expression()" to detect a "keys(...)"
 * reference at each position (the same primitive used to expand such references into M code elsewhere).
 */
int get_keys_in_global(char *global_source, SqlTable *table, SqlColumn **key_columns) {
	char *source_ptr;
	int   num_keys;

	num_keys = 0;
	source_ptr = global_source;
	while ('\0' != *source_ptr) {
		char		    column[OCTO_MAX_IDENT + 1]; // Null terminator
		char		    prev;
		int		    expr_len;
		ExpressionMatchType match;

		if (global_source == source_ptr) {
			/* "match_expression()" decides whether a "keys(" reference is a real subscript (versus part of a
			 * larger token) by looking at the preceding character. At the start of the string there is no
			 * preceding character, so pass '(' when the string itself starts with 'k'/'v' to allow the match.
			 */
			prev = ((('k' == *source_ptr) || ('v' == *source_ptr)) ? '(' : *source_ptr);
		} else {
			prev = *(source_ptr - 1);
		}

		match = match_expression(source_ptr, column, &expr_len, sizeof(column), prev);
		if (KeysExpression == match) {
			SqlColumn *key_column;

			/* The GLOBAL keyword was validated at CREATE TABLE time ("validate_global_keyword.c"), so
			 * every "keys(...)" reference here names a real key column of the table.
			 */
			key_column = find_column(column, table);
			assert(NULL != key_column);
			assert(num_keys < MAX_KEY_COUNT);
			key_columns[num_keys] = key_column;
			num_keys++;
			source_ptr += expr_len;
		} else {
			source_ptr++;
		}
	}

	return num_keys;
}
