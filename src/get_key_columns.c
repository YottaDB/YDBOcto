/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/**
 * Returns the maximum KEY NUM set (>= 0).
 * Populates key_columns[i] with a pointer to the column for that key.
 * key_columns must not be NULL.
 *
 * If there are no keys defined in table, returns -1
 * Other errors return -2
 */
int get_key_columns(SqlTable *table, SqlColumn **key_columns) {
	int		    key_num, max_key = -1, error = 0;
	SqlColumn *	    start_column, *cur_column;
	SqlOptionalKeyword *keyword;
	SqlValue *	    value;

	assert(NULL != key_columns);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		/* Ignore table-level constraints as a table-level PRIMARY KEY constraint would have already added the needed
		 * KEY NUM keywords to the respective columns. Hence the "NULL" check on "columnName" below.
		 */
		if (NULL != cur_column->columnName) {
			keyword = get_keyword(cur_column, OPTIONAL_KEY_NUM);
			if (NULL != keyword) {
				UNPACK_SQL_STATEMENT(value, keyword->v, value);
				key_num = atoi(value->v.string_literal);
				/* Note that it is possible "key_num" can be negative in case KEY NUM is a huge positive number.
				 * So handle that also as a positive number case below.
				 */
				if ((MAX_KEY_COUNT <= key_num) || (0 > key_num)) {
					SqlValue *tblName;

					UNPACK_SQL_STATEMENT(tblName, table->tableName, value);
					ERROR(ERR_TOO_MANY_TABLE_KEYCOLS, tblName->v.reference, value->v.string_literal,
					      MAX_KEY_COUNT - 1);
					return -2;
				}
				if (NULL != key_columns[key_num]) {
					UNPACK_SQL_STATEMENT(value, table->tableName, value);
					ERROR(ERR_MULTIPLE_ZERO_KEYS, key_num, value->v.reference);
					return -2;
				}
				key_columns[key_num] = cur_column;
				if (key_num > max_key) {
					max_key = key_num;
				}
			}
		}
		cur_column = cur_column->next;
	} while (start_column != cur_column);
	// check that all keys <= max_key have been initialized
	for (key_num = 0; key_num <= max_key; key_num++) {
		if (NULL == key_columns[key_num]) {
			UNPACK_SQL_STATEMENT(value, table->tableName, value);
			ERROR(ERR_MISSING_KEY, key_num, value->v.reference, max_key);
			error = 1;
		}
	}
	// if an error has been detected return that otherwise return max_key
	return error ? -2 : max_key;
}
