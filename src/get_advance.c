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
 * Fills buffer with a string representation of this columns advance
 *
 * Requires a list of key columns and their names
 */
int get_advance(char *buffer, int buffer_size, SqlColumn *column, SqlColumn **key_columns, char **key_names, SqlTable *table) {
	SqlOptionalKeyword *keyword;
	SqlValue *value;
	char *buff_ptr = buffer, key_name[MAX_STR_CONST], *temp;
	int i =0;

	keyword = get_keyword(column, OPTIONAL_ADVANCE);
	if(keyword) {
		UNPACK_SQL_STATEMENT(value, keyword->v, value);
		temp = m_unescape_string(value->v.string_literal);
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_size, "%s", temp);
		free(temp);
	} else {
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_size, "$O(^%s(", value->v.reference);
		for(i = 0; i <= MAX_KEY_COUNT && key_columns[i] != NULL; i++) {
			if(i != 0)
				SAFE_SNPRINTF(buff_ptr, buffer, buffer_size, ",");
			SAFE_SNPRINTF(buff_ptr, buffer, buffer_size, "$G(%s)", key_names[i]);
			if(key_columns[i] == column)
				break;
		}
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_size, "))");
	}
	*buff_ptr = '\0';
	return buff_ptr - buffer;
}
