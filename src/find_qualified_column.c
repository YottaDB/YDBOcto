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
 * Searches through the provided list of SqlJoin for the column, and populates table and column with pointers to the corresponding
 * data structures
 *
 * @returns 0 if everything is OK, 1 otherwise
 *
 * @param column_reference [in] qualified name of column to search for
 * @param table [out] the table referred to by column_reference
 * @param column [out] the column referred to by column reference
 */
int find_qualified_column(SqlValue *column_reference, SqlTable **table, SqlColumn **column) {
	char *table_name, *column_name;

	column_name = table_name = column_reference->v.reference;
	while(*column_name != '\0' && *column_name != '.')
		column_name++;
	assert(*column_name == '.');
	*column_name = '\0';
	*table = find_table(table_name);
	if(*table == NULL)
		return 1;
	*column_name++ = '.';
	*column = find_column(column_name, *table);
	if(*column == NULL)
		return 1;
	return 0;
}
