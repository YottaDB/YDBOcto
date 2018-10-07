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

int generate_cursor(char *buffer, int buffer_size, SqlTable *table) {
	int key_num, num_printed = 0, max_key = 0, i;
	char *advance = NULL, buff[MAX_STR_CONST], buff2[MAX_STR_CONST], *buffer_ptr;
	char *key_names[MAX_KEY_COUNT];
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
	max_key = get_key_columns(table, key_columns);
	key_names[max_key + 1] = "keys(\"_fakeKey\")";

	key_num = 0;
	buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "SET ");
	while(key_num <= max_key) {
		key_names[key_num] = malloc(MAX_STR_CONST);
		generate_key_name(key_names[key_num], MAX_STR_CONST, key_num, table, key_columns);
		if(key_num != 0)
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), ",");
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "oldKey%d=$G(%s)", key_num, key_names[key_num]);
		key_num++;
	}
	buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), " SET ");
	key_num = 0;
	while(key_num <= max_key) {
		if(key_columns[key_num] == NULL) {
			UNPACK_SQL_STATEMENT(value, table->tableName, value);
			ERROR(ERR_MISSING_KEY, key_num, value->v.reference, max_key);
			return 1;
		}
		if(key_num != 0)
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), ",");
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "%s=", key_names[key_num]);
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
				advance += snprintf(advance, MAX_STR_CONST - (advance - buff), "$G(%s)", key_names[i]);
			}
			advance += snprintf(advance, MAX_STR_CONST - (advance - buff), "))");
			*advance = '\0';
			advance = buff;
		}
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "$S(");
		if(key_num != 0) {
			generate_null_check(buff2, MAX_STR_CONST, table, key_num-1);
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "%s:\"\",", buff2);
		}
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "$G(%s)=\"\":%s,1:$G(%s))",
			key_names[key_num + 1], advance, key_names[key_num]);
		key_num++;
	}
	// If nothing changed, mark all keys as NULL
	buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), " SET:");
	key_num = 0;
	while(key_num <= max_key) {
		if(key_num != 0)
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "&");
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "(oldKey%d=$G(%s))", key_num, key_names[key_num]);
		key_num++;
	}
	buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), " ");
	key_num = 0;
	while(key_num <= max_key) {
		if(key_num != 0)
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), ",");
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "%s=\"\"", key_names[key_num]);
		free(key_names[key_num]);
		key_num++;
	}
	*buffer_ptr = '\0';
	if(buffer_ptr - buffer == 0)
		return 1;
	return 0;
}
