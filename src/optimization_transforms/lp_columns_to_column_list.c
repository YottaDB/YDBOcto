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

SqlColumnListAlias *lp_columns_to_column_list(SqlColumn *column, SqlTableAlias *table_alias) {
	SqlColumnList *cur, *t_column_list;
	SqlColumnListAlias *ret = NULL, *t_column_list_alias, *cur_column_list_alias;
	SqlStatement *stmt;
	SqlColumnAlias *alias;
	SqlColumn *cur_column, *start_column;

	if(column == NULL)
		return NULL;

	cur_column = start_column = column;
	do {
		cur = (SqlColumnList*)octo_cmalloc(memory_chunks, sizeof(SqlColumnList));
		memset(cur, 0, sizeof(SqlColumnList));
		dqinit(cur);
		SQL_STATEMENT(stmt, column_alias_STATEMENT);
		MALLOC_STATEMENT(stmt, column_alias, SqlColumnAlias);
		cur->value = stmt;

		alias = stmt->v.column_alias;
		PACK_SQL_STATEMENT(alias->column, cur_column, column);
		PACK_SQL_STATEMENT(alias->table_alias, table_alias, table_alias);

		cur_column_list_alias = (SqlColumnListAlias*)octo_cmalloc(memory_chunks, sizeof(SqlColumnListAlias));
		memset(cur_column_list_alias, 0, sizeof(SqlColumnListAlias));
		cur_column_list_alias->alias = cur_column->columnName;
		PACK_SQL_STATEMENT(cur_column_list_alias->column_list, cur, column_list);
		dqinit(cur_column_list_alias);
		PACK_SQL_STATEMENT(alias->table_alias, table_alias, table_alias);
		if(ret == NULL) {
			ret = cur_column_list_alias;
		} else {
			dqinsert(ret, cur_column_list_alias, t_column_list_alias);
		}
		cur_column = cur_column->next;
	} while(cur_column != start_column);
	assert(ret != NULL);
	return ret;
}
