/****************************************************************
 *								*
 * Copyright (c) 2021-2026 YottaDB LLC and/or its subsidiaries.	*
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

/* Validates the GLOBAL keyword for "keys()" and "values()" usages and issues errors as appropriate.
 * Returns
 *   -1 in case validation failed.
 *    0 otherwise (i.e. success).
 */
int validate_global_keyword(SqlOptionalKeyword *keyword, SqlTable *table, int max_key, boolean_t is_table) {
	char	 *ptr, *ptr_start;
	char	 *start, *next;
	int	  next_key_num;
	SqlValue *value;
	boolean_t key_is_iterator[MAX_KEY_COUNT];

	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	start = value->v.string_literal;
	next = strchr(start, '(');
	if (NULL == next) {
		/* GLOBAL keyword only specifies global name. No subscripts.
		 * No need to do any keys()/values() checks in that case. Return.
		 */
		return 0;
	}

	/* Build a per-key-num lookup of which KEY columns are ITERATOR columns.
	 * ITERATOR key columns are exempt from the "must appear in GLOBAL keys()" requirement
	 * because Octo iterates over their subscript at query time rather than matching it
	 * against a user-supplied value.
	 */
	memset(key_is_iterator, 0, sizeof(key_is_iterator));
	if (is_table) {
		SqlColumn *start_column, *cur_column;
		UNPACK_SQL_STATEMENT(start_column, table->columns, column);
		cur_column = start_column;
		do {
			if (NULL != cur_column->columnName) {
				SqlOptionalKeyword *kn_kw, *it_kw;

				kn_kw = get_keyword(cur_column, OPTIONAL_KEY_NUM);
				it_kw = get_keyword(cur_column, OPTIONAL_ITERATOR);
				if ((NULL != kn_kw) && (NULL != it_kw)) {
					SqlValue *kn_val;
					int	  kn_idx;

					UNPACK_SQL_STATEMENT(kn_val, kn_kw->v, value);
					kn_idx = atoi(kn_val->v.string_literal);
					assert((0 <= kn_idx) && (MAX_KEY_COUNT > kn_idx));
					key_is_iterator[kn_idx] = TRUE;
				}
			}
			cur_column = cur_column->next;
		} while (cur_column != start_column);
	}
	ptr_start = ptr = next + 1;
	next_key_num = 0;
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
				keyword = get_keyword(sql_column, OPTIONAL_KEY_NUM);
				assert(NULL != keyword);
				UNPACK_SQL_STATEMENT(value, keyword->v, value);

				int key_num;
				key_num = atoi(value->v.string_literal);
				// These asserts are valid even if we are checking a column, even if from another table unrelated
				// to this table. We don't expect to have keys repeated.
				assert(MAX_KEY_COUNT > key_num);
				assert(key_num <= max_key);
				/* In a column, it's possible for the user to use the keys in a different global; we should not
				 * enforce any expected order. While it possible for us to check if we are using the same global
				 * and issue an error in that case, this use case is considered advanced and users can debug
				 * their own column key resolution in that case rather than us issuing the error.
				 */
				if (is_table) {
					/* Advance past any ITERATOR key columns the user is allowed to skip.
					 * An ITERATOR key column may be omitted from GLOBAL but is still accepted
					 * when explicitly mentioned, which is why we stop advancing the moment
					 * the expected slot matches the user-supplied key_num.
					 */
					while ((next_key_num <= max_key) && key_is_iterator[next_key_num]
					       && (next_key_num != key_num)) {
						next_key_num++;
					}
					if (key_num != next_key_num) {
						ERROR(ERR_GLOBAL_KEY_COLS_ORDER, "");
						return -1;
					}
				}
				next_key_num++;
			} else {
				/* "values()" syntax used. This is disallowed in GLOBAL keyword. */
				assert('v' == *ptr);
				ERROR(ERR_VALUES_NOT_ALLOWED_IN_GLOBAL, "");
				return -1;
			}
			assert(0 < expr_len);
			ptr += expr_len;
		} else {
			ptr++;
		}
	}

	/* The following check ONLY applies if we are checking the GLOBAL keyword on a table, where it indeed has to match the
	 * primary keys exactly. For columns, it's possible to use LESS keys than the full table. E.g.
	 *
	 * ^table(1)="value1"
	 * ^table(1,1)="value2"
	 *
	 * The table may have two columns, but a column can be defined which only uses one key to get "value1".
	 *
	 * ITERATOR key columns are also exempt: they may be omitted from GLOBAL because Octo iterates over
	 * their subscript at query time. Walk past any trailing ITERATOR slots before comparing.
	 */
	if (is_table) {
		while ((next_key_num <= max_key) && key_is_iterator[next_key_num]) {
			next_key_num++;
		}
		if (next_key_num != (max_key + 1)) {
			ERROR(ERR_GLOBAL_MISSING_KEY_COLS, "");
			return -1;
		}
	}
	return 0;
}
