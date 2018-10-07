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
 * Generates a string to check values up to max_key_null_check for lack of value
 *
 * If max_key_null_check is less than 0, checks all keys
 */
int generate_null_check(char *buffer, int buffer_size, SqlTable *table, int max_key_null_check) {
	char *buff_ptr = buffer, *key_name;
	int cur_key = 0, max_key = 0;
	SqlColumn *key_columns[MAX_KEY_COUNT], *column;


	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);
	if(max_key_null_check >= 0)
	  max_key = max_key_null_check;
	key_name = malloc(MAX_STR_CONST);

	for(; cur_key <= max_key; cur_key++) {
		generate_key_name(key_name, MAX_STR_CONST, cur_key, table, key_columns);
		if(cur_key != 0) {
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "!");
			if(buff_ptr - buffer >= buffer_size)
				return 1;
		}
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "($G(%s)=\"\")", key_name);
		if(buff_ptr - buffer >= buffer_size)
			return 1;
	}
	free(key_name);
	*buff_ptr = '\0';
	return 0;
}
