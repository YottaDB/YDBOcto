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
#include <string.h>

#include "octo.h"

/* Returns how many key columns of "table" the GLOBAL keyword of "column" lists ("get_keys_in_global()"), when they are the
 * first key columns of the table in key order (KEY NUM). For example, with PRIMARY KEY (a, b):
 *   - "^R(keys(""a""))" gives 1 and "^R(keys(""a""),keys(""b""))" gives 2;
 *   - a global with no "keys(...)" subscript, such as "^R", gives 0;
 *   - a column with no GLOBAL keyword of its own gives the table's number of key columns (2): its value is in the table's
 *     GLOBAL, which lists every key column in key order (ERR_GLOBAL_KEY_COLS_ORDER).
 * Returns -1 when the GLOBAL lists the key columns in another order (e.g. "^R(keys(""b""),keys(""a""))") or skips one
 * (e.g. "^R(keys(""b""))"). An AIM cross reference of such a column cannot be walked for the rows of the table: a WHERE
 * ("get_xref_advance_keys()") and MIN/MAX ("is_octo617_optimization_possible()") then read every row instead.
 */
int get_column_global_key_prefix(SqlColumn *column, SqlTable *table) {
	SqlOptionalKeyword *global_keyword;
	SqlValue	   *value;
	SqlColumn	   *key_columns[MAX_KEY_COUNT], *global_key_columns[MAX_KEY_COUNT];
	int		    num_key_columns, num_global_keys, key_num;

	memset(key_columns, 0, sizeof(key_columns)); /* "get_key_columns()" expects every entry to be NULL */
	num_key_columns = get_key_columns(table, key_columns) + 1;
	assert(0 < num_key_columns); /* The table was validated when it was created */

	global_keyword = get_keyword(column, OPTIONAL_GLOBAL);
	if (NULL == global_keyword) {
		return num_key_columns; /* The column's value is in the table's GLOBAL */
	}
	UNPACK_SQL_STATEMENT(value, global_keyword->v, value);
	num_global_keys = get_keys_in_global(value->v.string_literal, table, global_key_columns);
	/* CREATE TABLE allows only the table's key columns in a GLOBAL. Were there more, the loop below would still return -1,
	 * as "key_columns" holds NULL past the last key column.
	 */
	assert(num_global_keys <= num_key_columns);
	for (key_num = 0; key_num < num_global_keys; key_num++) {
		if (global_key_columns[key_num] != key_columns[key_num]) {
			return -1; /* Not the first key columns of the table, in key order */
		}
	}
	return num_global_keys;
}
