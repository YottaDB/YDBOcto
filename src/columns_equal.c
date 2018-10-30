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

int columns_equal(SqlColumn *a, SqlColumn *b) {
	SqlValue *valA, *valB;
	SqlTable *tableA, *tableB;
	if(a->type != b->type)
		return FALSE;
	UNPACK_SQL_STATEMENT(valA, a->columnName, value);
	UNPACK_SQL_STATEMENT(valB, b->columnName, value);
	if(values_equal(valA, valB) == FALSE)
		return FALSE;
	UNPACK_SQL_STATEMENT(tableA, a->table, table);
	UNPACK_SQL_STATEMENT(tableB, b->table, table);
	if(tables_equal(tableA, tableB) == FALSE)
		return FALSE;
	// Note, we don't compare keywords because it is highly unlikely that they would
	//  be different, and that would be a bad state, but expensive to do
	//  It could be added as a debug check
	return TRUE;
}
