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

int generate_null_check(char *buffer, int buffer_size, int max_key) {
	char *buff_ptr = buffer;
	int cur_key = 0;

	for(; cur_key < max_key; cur_key++) {
		if(cur_key != 0) {
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "!");
			if(buff_ptr - buffer >= buffer_size)
				return 1;
		}
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "(keys(%d)=\"\")", cur_key);
		if(buff_ptr - buffer >= buffer_size)
			return 1;
	}
	*buff_ptr = '\0';
	return 0;
}

int generate_cursor(char *buffer, int buffer_size, SqlTable *table) {
	int key_num, num_printed = 0, max_key = 0, i;
	char *advance = NULL, buff[MAX_STR_CONST], buff2[MAX_STR_CONST], *buffer_ptr;
	SqlOptionalKeyword *keyword;
	SqlColumn *key_columns[MAX_KEY_COUNT], *start_column, *cur_column;
	SqlValue *value;

	buffer_ptr = buffer;
	// Simpliest option; return the CURSOR if specified
	if(table->curse && table->curse->v.keyword->v) {
		UNPACK_SQL_STATEMENT(value, table->curse->v.keyword->v, value);
		strncpy(buffer, value->v.reference, buffer_size);
		buffer[buffer_size-1] = '\0';
		return 0;
	}

	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;

	do {
		keyword = get_keyword(cur_column, PRIMARY_KEY);
		if(keyword != NULL) {
			if(key_columns[0] != NULL) {
				UNPACK_SQL_STATEMENT(value, table->tableName, value);
				ERROR(ERR_MULTIPLE_ZERO_KEYS, 0, value->v.reference);
				return 1;
			}
			key_columns[0] = cur_column;
		}
		keyword = get_keyword(cur_column, OPTIONAL_KEY_NUM);
		if(keyword != NULL) {
			UNPACK_SQL_STATEMENT(value, keyword->v, value);
			key_num = atoi(value->v.string_literal);
			if(key_columns[key_num] != NULL) {
				UNPACK_SQL_STATEMENT(value, table->tableName, value);
				ERROR(ERR_MULTIPLE_ZERO_KEYS, key_num, value->v.reference);
				return 1;
			}
			key_columns[key_num] = cur_column;
			if(key_num > max_key)
				max_key = key_num;
		}
		cur_column = cur_column->next;
	} while(start_column != cur_column);

	key_num = max_key;
	buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "SET ");
	while(key_num >= 0) {
		if(key_columns[key_num] == NULL) {
			UNPACK_SQL_STATEMENT(value, table->tableName, value);
			ERROR(ERR_MISSING_KEY, key_num, value->v.reference, max_key);
			return 1;
		}
		if(key_num != max_key)
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), ",");
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "keys(%d)=", key_num);
		keyword = get_keyword(key_columns[key_num], OPTIONAL_ADVANCE);
		if(keyword) {
			UNPACK_SQL_STATEMENT(value, keyword->v, value);
			advance = value->v.string_literal;
		} else {
			advance = buff;
			UNPACK_SQL_STATEMENT(value, table->tableName, value);
			advance += snprintf(advance, MAX_STR_CONST, "$O(^%s(", value->v.reference);
			for(i = 0; i <= key_num; i++) {
				if(i != 0)
					advance += snprintf(advance, MAX_STR_CONST - (advance - buff), ",");
				advance += snprintf(advance, MAX_STR_CONST - (advance - buff), "keys(%d)", i);
			}
			advance += snprintf(advance, MAX_STR_CONST - (advance - buff), "))");
			*advance = '\0';
			advance = buff;
		}
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "$S(");
		if(key_num != 0) {
			generate_null_check(buff2, MAX_STR_CONST, key_num);
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "%s:\"\",", buff2);
		}
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "$G(keys(%d))=\"\":%s,1:keys(%d))",
			key_num + 1, advance, key_num);
		key_num--;
	}
	if(num_printed == 0)
		return 1;
	return 0;
}
