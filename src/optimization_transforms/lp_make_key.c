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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_make_key(SqlColumnAlias *column_alias) {
	LogicalPlan *ret;
	SqlColumn *column;
	SqlTableAlias *table_alias;
	SqlTable *table;

	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias, table_alias);
	UNPACK_SQL_STATEMENT(table, table_alias->table, table);

	MALLOC_LP(ret, LP_KEY);
	ret->v.key = (SqlKey*)malloc(sizeof(SqlKey));
	memset(ret->v.key, 0, sizeof(SqlKey));
	ret->v.key->column = column;
	ret->v.key->key_num = -1;
	ret->v.key->random_id = table_alias->unique_id;
	ret->v.key->table = table;
	ret->v.key->type = LP_KEY_ADVANCE;
	return ret;
}
