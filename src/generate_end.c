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

int generate_end(char *buffer, int buffer_size, SqlTable *table) {
	int key_num, num_printed = 0, max_key = 0, i;
	char *advance = NULL, buff[MAX_STR_CONST], buff2[MAX_STR_CONST], *buffer_ptr;
	char *key_names[MAX_KEY_COUNT];
	SqlOptionalKeyword *keyword;
	SqlColumn *key_columns[MAX_KEY_COUNT], *column;
	SqlValue *value;

	buffer_ptr = buffer;
	// Simpliest option; return the CURSOR if specified
	if(table->end && table->end->v.keyword->v) {
		UNPACK_SQL_STATEMENT(value, table->end->v.keyword->v, value);
		strncpy(buffer, value->v.reference, buffer_size);
		buffer[buffer_size-1] = '\0';
		return 0;
	}

	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);

	key_num = max_key;
	while(key_num >= 0) {
		// This could be replaced with a single char*, rather than array, but staying consistent
		//  makes it easier at this point
		key_names[key_num] = malloc(MAX_STR_CONST);
		generate_key_name(key_names[key_num], MAX_STR_CONST, key_num, table, key_columns);
		if(key_num != max_key)
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "&");
		column = key_columns[key_num];
		UNPACK_SQL_STATEMENT(value, column->columnName, value);
		emit_simple_select(buff, table, value->v.reference, NULL);
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "((%s=\"\")", buff);
		keyword = get_keyword(column, OPTIONAL_ADVANCE);
		if(keyword != NULL) {
			UNPACK_SQL_STATEMENT(value, keyword->v, value);
			buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "!(%s=%s)", buff, value->v.string_literal);
		}
		buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), ")");
		free(key_names[key_num]);
		key_num--;
	}
	*buffer_ptr = '\0';
	if(buffer_ptr - buffer == 0)
		return 1;
	return 0;
}
