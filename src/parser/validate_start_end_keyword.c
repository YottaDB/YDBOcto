/****************************************************************
 *								*
 * Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Validates the START/END keyword for "keys()" and "values()" usages and issues errors as appropriate.
 * Note: A lot of the below code is similar to that in validate_global_keyword.c.
 * Returns
 *   -1 in case validation failed.
 *    0 otherwise (i.e. success).
 */
int validate_start_end_keyword(SqlOptionalKeyword *keyword, SqlTable *table) {
	char	 *ptr, *ptr_start;
	char	 *start;
	SqlValue *value;

	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	start = value->v.string_literal;
	ptr_start = ptr = start;
	/* Note: The below logic is similar to that in "tmpl_emit_source.ctemplate" */
	while ('\0' != *ptr) {
		char		    prev;
		char		    column[OCTO_MAX_IDENT + 1]; // Null terminator
		int		    expr_len;
		ExpressionMatchType match;

		if (ptr_start == ptr) {
			/* See comment in "tmpl_emit_source.ctemplate" for why these checks are done */
			prev = ((('k' == *ptr) || ('v' == *ptr)) ? '(' : *ptr);
		} else {
			prev = *(ptr - 1);
		}
		match = match_expression(ptr, column, &expr_len, sizeof(column), prev);
		if (NoMatchExpression < match) {
			SqlColumn *sql_column;

			sql_column = find_column(column, table);
			if (NULL == sql_column) {
				/* Check if YDBOcto#929 conversion is needed (lower case column name) */
				DO_AUTO_UPGRADE_OCTO929_CHECK(ptr, expr_len, column, sql_column);
				if (NULL == sql_column) {
					ERROR(ERR_UNKNOWN_COLUMN_NAME, column);
					return -1;
				}
			}
			if ('k' == *ptr) {
				/* "keys()" syntax used. Check that this column is a KEY column. */
				if (!IS_KEY_COLUMN(sql_column)) {
					ERROR(ERR_KEYS_NEEDS_A_KEY_COLUMN, column);
					return -1;
				}
			} else {
				/* "values()" syntax used. This is disallowed in START/END keywords. */
				assert('v' == *ptr);
				ERROR(ERR_VALUES_NOT_ALLOWED_IN_START_END, "");
				return -1;
			}
			assert(0 < expr_len);
			ptr += expr_len;
		} else {
			ptr++;
		}
	}
	return 0;
}
