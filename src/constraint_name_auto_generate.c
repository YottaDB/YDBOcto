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
 * 1) "constraint_type" is the type of constraint we are dealing with (e.g. OPTIONAL_CHECK_CONSTRAINT).
 * 2) "table_name" and "column_name" are the names of the table and column corresponding to the constraint.
 * 3) "numeric_suffix" is a number that is added at the end of the constraint name for uniqueness.
 *    If it is 0, no suffix is added. If it is 1, a "_1" suffix is added. If it is 2, a "_2" suffix is added and so on.
 *
 * Output
 * ------
 * 1) "ret_buf" is the buffer where the resulting constraint name is written.
 * 2) "ret_buf_size" is the size of the allocated buffer and the resulting constraint name length
 *    (including the null terminating byte) is guaranteed to not exceed this allocated length.
 */
void constraint_name_auto_generate(OptionalKeyword constraint_type, char *table_name, char *column_name, int numeric_suffix,
				   char *ret_buf, int ret_buf_size) {
	int len, table_name_len, reserved;

	switch (constraint_type) {
	case OPTIONAL_CHECK_CONSTRAINT:
		/* In all if/else code paths below, check return value of "snprintf" and handle the case
		 * if space is not enough by truncating the column name and/or table name parts of the generated name
		 * so we have space for the "_check" and numeric suffix at the end.
		 */
		if (0 == numeric_suffix) {
			if (NULL == column_name) {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s", table_name, OCTOLIT_CHECK);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s_%s", table_name, column_name, OCTOLIT_CHECK);
			}
		} else {
			if (NULL == column_name) {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s%d", table_name, OCTOLIT_CHECK, numeric_suffix);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%s_%s_%s%d", table_name, column_name, OCTOLIT_CHECK,
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
		reserved = sizeof(OCTOLIT_CHECK) + 1; /* + 1 is for underscore, sizeof() already counts space for null terminator */
		if (0 != numeric_suffix) {
			reserved += snprintf(ret_buf, ret_buf_size, "%d", numeric_suffix);
		}
		if (NULL == column_name) {
			/* It is a table-level constraint */
			table_name_len = ret_buf_size - reserved;
			if (0 == numeric_suffix) {
				len = snprintf(ret_buf, ret_buf_size, "%.*s_%s", table_name_len, table_name, OCTOLIT_CHECK);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%.*s_%s%d", table_name_len, table_name, OCTOLIT_CHECK,
					       numeric_suffix);
			}
		} else {
			/* It is a column-level constraint */
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
					       column_name, OCTOLIT_CHECK);
			} else {
				len = snprintf(ret_buf, ret_buf_size, "%.*s_%.*s_%s%d", table_name_len, table_name, column_name_len,
					       column_name, OCTOLIT_CHECK, numeric_suffix);
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
