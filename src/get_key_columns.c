/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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
 * Returns the maximum KEY NUM set (>= 0). If key_columns is not null, populates key_columns[i] with a pointer to the column for that key
 *
 * If there are no keys defined in table, returns -1
 */
int get_key_columns(SqlTable *table, SqlColumn **key_columns) {
	int key_num, max_key = -1;
	SqlColumn *start_column, *cur_column;
	SqlOptionalKeyword *keyword;
	SqlValue *value;
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;

	do {
		keyword = get_keyword(cur_column, PRIMARY_KEY);
		if(keyword != NULL && key_columns != NULL) {
			if(key_columns[0] != NULL) {
				UNPACK_SQL_STATEMENT(value, table->tableName, value);
				ERROR(ERR_MULTIPLE_ZERO_KEYS, 0, value->v.reference);
				return 1;
			}
			key_columns[0] = cur_column;
			max_key = 0;
		}
		keyword = get_keyword(cur_column, OPTIONAL_KEY_NUM);
		if(keyword != NULL) {
			UNPACK_SQL_STATEMENT(value, keyword->v, value);
			key_num = atoi(value->v.string_literal);
			if(key_columns != NULL && key_columns[key_num] != NULL) {
				UNPACK_SQL_STATEMENT(value, table->tableName, value);
				ERROR(ERR_MULTIPLE_ZERO_KEYS, key_num, value->v.reference);
				return 1;
			}
			if(key_columns != NULL)
				key_columns[key_num] = cur_column;
			if(key_num > max_key)
				max_key = key_num;
		}
		cur_column = cur_column->next;
		assert(max_key < MAX_KEY_COUNT);
	} while(start_column != cur_column);
	return max_key;
}
