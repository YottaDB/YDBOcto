/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
