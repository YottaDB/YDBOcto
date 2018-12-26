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

int lp_get_key_index(LogicalPlan *plan, LogicalPlan *lp_column_alias) {
  	SqlColumnAlias *column_alias;
	SqlColumn *column;
	SqlValue *key_table_name, *search_table_name, *key_column_name, *search_column_name;
	SqlTableAlias *table_alias;
	SqlTable *table;
	SqlColumnListAlias *cl_alias;
	SqlKey *key;
	int key_id, search_id, cur_key_index;
	LogicalPlan *cur_key, *lp_key;

	column_alias = lp_column_alias->v.column_alias;
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias, table_alias);
	search_id = table_alias->unique_id;
	UNPACK_SQL_STATEMENT(table, table_alias->table, table);
	UNPACK_SQL_STATEMENT(search_table_name, table->tableName, value);

	if(column_alias->column->type == column_STATEMENT) {
		UNPACK_SQL_STATEMENT(column, column_alias->column, column);
		UNPACK_SQL_STATEMENT(search_column_name, column->columnName, value);
	} else {
		UNPACK_SQL_STATEMENT(cl_alias, column_alias->column, column_list_alias);
		UNPACK_SQL_STATEMENT(search_column_name, cl_alias->alias, value);
	}

	cur_key = lp_get_keys(plan);
	cur_key_index = 0;

	do {
		GET_LP(lp_key, cur_key, 0, LP_KEY);
		key = lp_key->v.key;
		key_id = key->random_id;
		UNPACK_SQL_STATEMENT(key_table_name, key->table->tableName, value);
		UNPACK_SQL_STATEMENT(key_column_name, key->column->columnName, value);
		do {
			if(key_id != search_id)
				break;
			if(strcmp(search_table_name->v.string_literal, key_table_name->v.string_literal) != 0)
				break;
			if(strcmp(search_column_name->v.string_literal, key_column_name->v.string_literal) != 0)
				break;
			return cur_key_index;
		} while(TRUE);
		cur_key = cur_key->v.operand[1];
		cur_key_index++;
	} while(cur_key != NULL);
	return -1;
}
