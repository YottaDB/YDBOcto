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
 * Populates buffer with the M representation of the specified target_key_num for this key from table
 *
 * @returns the number of characters written
 */
int generate_key_name(char *buffer, int buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns) {
	char *buffer_ptr = buffer, *columnName, *temp;
	int written;
	SqlValue *value;
	SqlOptionalKeyword *keyword;

	UNUSED(table); // we may eventually need this, and it's already in the code

	if(key_columns[target_key_num] == NULL)
		return 0;
	keyword = get_keyword(key_columns[target_key_num], OPTIONAL_EXTRACT);
	if(keyword != NULL) {
		UNPACK_SQL_STATEMENT(value, keyword->v, value);
		temp = m_unescape_string(value->v.string_literal);
		SAFE_SNPRINTF(written, buffer_ptr, buffer, buffer_size, "%s", temp);
		free(temp);
		return buffer_ptr - buffer;
	}
	UNPACK_SQL_STATEMENT(value, key_columns[target_key_num]->columnName, value);
	columnName = value->v.reference;

	buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "keys(\"%s\")", columnName);
	*buffer_ptr++ = '\0';

	return buffer_ptr - buffer;
}
