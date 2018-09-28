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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

SqlTable *find_table(const char *table_name) {
	SqlValue *tmp_value;
	SqlTable *start_table, *cur_table;

	TRACE(CUSTOM_ERROR, "Searching for table %s",
	      table_name);

	if(definedTables == NULL)
		return NULL;
	start_table = cur_table = definedTables;
	do {
		UNPACK_SQL_STATEMENT(tmp_value, cur_table->tableName, value);
		if(strcmp(tmp_value->v.reference, table_name) == 0) {
			return cur_table;
		}
		cur_table = cur_table->next;
	} while(start_table != cur_table);
	return NULL;
}
