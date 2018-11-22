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
#include <string.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

SqlTable *copy_sql_table(SqlTable *table) {
	SqlTable *ret;
	SqlStatement *stmt;
	SqlColumn *cur_column, *start_column, *new_column, *t_column;

	ret = (SqlTable*)malloc(sizeof(SqlTable));
	memset(ret, 0, sizeof(SqlTable));

	if(table->tableName) {
		ret->tableName = copy_sql_statement(table->tableName);
	}
	if(table->source) {
		ret->source = copy_sql_statement(table->source);
	}
	if(table->columns) {
		SQL_STATEMENT(stmt, column_STATEMENT);
		UNPACK_SQL_STATEMENT(start_column, table->columns, column);
		cur_column = start_column;
		do {
			new_column = (SqlColumn*)malloc(sizeof(SqlColumn));
			*new_column = *cur_column;
			dqinit(new_column);
			new_column->columnName = copy_sql_statement(cur_column->columnName);
			// This gives us trouble with a table copying the column itself;
			//  just setup a pointer
			//new_column->table = copy_sql_statement(cur_column->table);
			//SQL_STATEMENT(new_column->table, table_STATEMENT);
			//new_column->table->v.table = cur_column->table->v.table;
			//new_column->table = cur_column->table;
			new_column->keywords = copy_sql_statement(cur_column->keywords);
			if(stmt->v.column == NULL) {
				MALLOC_STATEMENT(stmt, column, SqlColumn);
				stmt->v.column = new_column;
			} else {
				dqinsert(new_column, stmt->v.column, t_column);
			}
			cur_column = cur_column->next;
		} while(cur_column != start_column);
		ret->columns = stmt;
		//ret->columns = copy_sql_statement(table->columns);
		PACK_SQL_STATEMENT(stmt, ret, table);
		assign_table_to_columns(stmt);
	}
	if(table->curse) {
		ret->curse = copy_sql_statement(table->curse);
	}
	if(table->start) {
		ret->start = copy_sql_statement(table->start);
	}
	if(table->end) {
		ret->end = copy_sql_statement(table->end);
	}
	if(table->delim) {
		ret->delim = copy_sql_statement(table->delim);
	}
	if(table->pack) {
		ret->pack = copy_sql_statement(table->pack);
	}
	dqinit(ret);

	return ret;
}
