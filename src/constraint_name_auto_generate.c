/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
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

/* This function auto generates a constraint name and returns the result in "name_buf".
 *
 * Input
 * -----
 * 1) "cur_keyword" is the keyword holding the constraint we are dealing with (e.g. OPTIONAL_CHECK_CONSTRAINT).
 * 2) "table_name" and "column_name" are the names of the table and column corresponding to the constraint.
 *    "column_name" is NULL in case it is a table-level constraint and non-NULL in case it is a column-level constraint.
 * 3) "numeric_suffix" is a number that is added at the end of the constraint name for uniqueness.
 *    If it is 0, no suffix is added. If it is 1, a "_1" suffix is added. If it is 2, a "_2" suffix is added and so on.
 *
 * Output
 * ------
 * 1) "ret_buf" is the buffer where the resulting constraint name is written.
 * 2) "ret_buf_size" is the size of the allocated buffer and the resulting constraint name length
 *    (including the null terminating byte) is guaranteed to not exceed this allocated length.
 */
void constraint_name_auto_generate(SqlOptionalKeyword *cur_keyword, char *table_name, char *column_name, int numeric_suffix,
				   char *ret_buf, int ret_buf_size) {
	int len, table_name_len, reserved;

	switch (cur_keyword->keyword) {
	case OPTIONAL_CHECK_CONSTRAINT:
	case UNIQUE_CONSTRAINT:;
		char  col_list_buf[OCTO_MAX_IDENT + 1]; /* + 1 for null terminator */
		char *suffix;

		if (OPTIONAL_CHECK_CONSTRAINT == cur_keyword->keyword) {
			suffix = OCTOLIT_CHECK;
		} else {
			assert(UNIQUE_CONSTRAINT == cur_keyword->keyword);

			SqlConstraint *constraint;
			UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
			assert(NULL == constraint->name);
			assert(NULL != constraint->definition); /* holds list of columns */

			/* First form the concatenated list of columns involved in the UNIQUE constraint.
			 * Limit it to a max of OCTO_MAX_IDENT characters.
			 */
			char *buf;
			int   buf_len, len;
			buf = col_list_buf;
			buf_len = sizeof(col_list_buf);

			SqlColumnList *start_cl, *cur_cl;
			UNPACK_SQL_STATEMENT(start_cl, constraint->definition, column_list);
			cur_cl = start_cl;
			do {
				SqlValue *col_name;
				UNPACK_SQL_STATEMENT(col_name, cur_cl->value, value);
				if (cur_cl == start_cl) {
					len = snprintf(buf, buf_len, "%s", col_name->v.string_literal);
				} else {
					len = snprintf(buf, buf_len, "_%s", col_name->v.string_literal);
				}
				if (len >= buf_len) {
					/* Concatenated list of column names is too long to be used in constraint name.
					 * Stop here and use a prefix of the concatenated list in the final constraint name.
					 */
					buf[buf_len - 1] = '\0'; /* null terminate concatenated list */
					break;
				}
				buf += len;
				buf_len -= len;
				cur_cl = cur_cl->next;
			} while (cur_cl != start_cl);
			/* Note: The concatenated list of column names of the UNIQUE constraint becomes the "column_name"
			 * for the logic below. Hence the re-assignment of the "column_name" variable.
			 */
			column_name = col_list_buf;
			assert(NULL != column_name);
			suffix = OCTOLIT_KEY;
		}
		/* In all if/else code paths below, check return value of "snprintf" and handle the case
		 * if space is not enough by truncating the column name and/or table name parts of the generated name
		 * so we have space for the "_check" and numeric suffix at the end.
		 */
		if (0 == numeric_suffix) {
			if (NULL == column_name) {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s", table_name, suffix);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s_%s", table_name, column_name, suffix);
			}
		} else {
			if (NULL == column_name) {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s%d", table_name, suffix, numeric_suffix);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s_%s%d", table_name, column_name, suffix,
					       numeric_suffix);
			}
		}
		if (len < ret_buf_size) {
			/* The auto generated constraint name fits in the return buffer. No more processing needed. */
			break;
		}
		/* The auto generated constraint name did not fit in the return buffer. Need to truncate some portions
		 * of the table/column name in order to generate a constraint name that fits in the return buffer.
		 */
		reserved = strlen(suffix) + 2; /* + 1 is for underscore, +1 for null terminator */
		if (0 != numeric_suffix) {
			reserved += snprintf(ret_buf, ret_buf_size, "%d", numeric_suffix);
		}
		if (NULL == column_name) {
			/* It is a table-level CHECK constraint */
			table_name_len = ret_buf_size - reserved;
			if (0 == numeric_suffix) {
				len = snprintf(ret_buf, ret_buf_size, "%.*s_%s", table_name_len, table_name, OCTOLIT_CHECK);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%.*s_%s%d", table_name_len, table_name, OCTOLIT_CHECK,
					       numeric_suffix);
			}
		} else {
			/* It is a column-level CHECK constraint or a table-level/column-level UNIQUE constraint */
			int avail, column_name_len, table_avail, column_avail;

			avail = ret_buf_size - reserved - 1; /* - 1 is for underscore between table and column name */
			table_name_len = strlen(table_name);
			column_name_len = strlen(column_name);
			/* If "avail" is an odd number, and both table and column names are long,
			 * allocate 1 extra byte for table name. This mirrors Postgres' behavior.
			 */
			column_avail = avail / 2;
			table_avail = avail - column_avail;
			if (table_name_len <= table_avail) {
				/* Table name needs no truncation. Only column name needs truncation. */
				column_name_len = avail - table_name_len;
			} else if (column_name_len <= column_avail) {
				/* Column name needs no truncation. Only table name needs truncation. */
				table_name_len = avail - column_name_len;
			} else {
				/* Both table and column name need truncation */
				if (column_name_len > column_avail) {
					column_name_len = column_avail;
				}
				table_name_len = table_avail;
			}
			if (0 == numeric_suffix) {
				len = snprintf(ret_buf, ret_buf_size, "%.*s_%.*s_%s", table_name_len, table_name, column_name_len,
					       column_name, suffix);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%.*s_%.*s_%s%d", table_name_len, table_name, column_name_len,
					       column_name, suffix, numeric_suffix);
			}
		}
		assert((len + 1) == ret_buf_size);
		UNUSED(len); /* needed to avoid a [clang-analyzer-deadcode.DeadStores] warning */
		break;
	default:
		assert(FALSE);
		break;
	}
	return;
}
