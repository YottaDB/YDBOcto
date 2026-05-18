/****************************************************************
 *								*
 * Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	*
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

/* Validates the START/END/SKIPCONDITION keyword for "keys()" and "values()" usages and issues errors as appropriate.
 * "cur_column" is the column on which the START/END/SKIPCONDITION keyword is specified; it lets us reject
 * "keys()" references to columns that are iterated at a deeper key level than "cur_column", since the cursor
 * for those deeper keys has not been set yet at the point the keyword's M expression executes. "cur_column"
 * may be NULL when the level check is not applicable; in that case the deeper-level check is skipped.
 *
 * Note: A lot of the below code is similar to that in validate_global_keyword.c.
 * Returns
 *   -1 in case validation failed.
 *    0 otherwise (i.e. success).
 */
int validate_start_end_keyword(SqlOptionalKeyword *keyword, SqlTable *table, SqlColumn *cur_column) {
	char	 *ptr, *ptr_start;
	char	 *start;
	SqlValue *value;

	/* Look up the KEY NUM of "cur_column" so we can reject forward "keys()" references later.
	 * If "cur_column" is not a key column (e.g. START/END accidentally placed on a non-key column),
	 * leave "cur_key_num" at -1 so the deeper-level check is skipped - the more general "must be on
	 * a key column" check is enforced by the caller for SKIP/SKIPCONDITION.
	 */
	int cur_key_num;
	cur_key_num = -1;
	if ((NULL != cur_column) && IS_KEY_COLUMN(cur_column)) {
		SqlOptionalKeyword *key_num_kw;
		SqlValue	   *key_num_value;

		key_num_kw = get_keyword(cur_column, OPTIONAL_KEY_NUM);
		UNPACK_SQL_STATEMENT(key_num_value, key_num_kw->v, value);
		cur_key_num = atoi(key_num_value->v.string_literal);
	}

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

				/* Reject forward "keys()" references: the referenced column's KEY NUM must be
				 * less than or equal to "cur_column"'s KEY NUM. A reference to a column with a
				 * higher KEY NUM reads its cursor before the corresponding inner $ORDER has set
				 * it, yielding LVUNDEF at runtime. See review comment:
				 *   https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1690#note_3377910381
				 */
				if (-1 != cur_key_num) {
					SqlOptionalKeyword *ref_key_num_kw;
					SqlValue	   *ref_key_num_value;
					int		    ref_key_num;

					ref_key_num_kw = get_keyword(sql_column, OPTIONAL_KEY_NUM);
					UNPACK_SQL_STATEMENT(ref_key_num_value, ref_key_num_kw->v, value);
					ref_key_num = atoi(ref_key_num_value->v.string_literal);

					if (ref_key_num > cur_key_num) {
						SqlValue *cur_column_name;

						UNPACK_SQL_STATEMENT(cur_column_name, cur_column->columnName, value);
						ERROR(ERR_KEYS_FORWARD_REFERENCE, column, cur_column_name->v.string_literal,
						      column);
						return -1;
					}
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
