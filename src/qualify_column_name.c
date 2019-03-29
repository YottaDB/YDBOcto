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

#include "logical_plan.h"

SqlStatement *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len);

/**
 * Tries to find the column in the list of tables
 * If the name is already qualifies, verifies the table exists.
 *
 * For the case of join tables, searches using the <tableName>.<columnName>
 *  followed by searching without seperating the two parts
 */
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables) {
	int table_name_len = 0, column_name_len = 0;
	char *table_name = NULL, *column_name = NULL, *c;
	SqlColumnAlias *ret;
	SqlStatement *column = NULL, *t_column;
	SqlTableAlias *cur_alias, *matching_alias;
	SqlJoin *cur_join, *start_join;
	SqlValue *value;


	// If the value is not a column_reference, we should not be here
	assert(column_value->type == COLUMN_REFERENCE);

	// Find the first period; if it is missing, we need to match against
	//  all columns in all tables
	for(c = column_value->v.string_literal; *c != '\0' && *c != '.'; c++) {
		// Pass
	}
	if(*c == '.') {
		table_name = column_value->v.reference;
		table_name_len = c - table_name;
		column_name = c+1;
		column_name_len = strlen(column_name);
	} else {
		column_name = column_value->v.reference;
		column_name_len = c - column_name;
	}

	cur_join = start_join = tables;
	do {
		// If we need to match a table, ensure this table
		//  is the correct one before calling the helper
		UNPACK_SQL_STATEMENT(cur_alias, cur_join->value, table_alias);
		if(table_name) {
			UNPACK_SQL_STATEMENT(value, cur_alias->alias, value);
			if(memcmp(value->v.reference, table_name, table_name_len) == 0) {
				matching_alias = cur_alias;
				column = match_column_in_table(cur_alias, column_name, column_name_len);
				break;
			}
		} else {
			t_column = match_column_in_table(cur_alias, column_name, column_name_len);
			if(t_column != NULL) {
				if(column != NULL) {
					WARNING(CUSTOM_ERROR, "Ambgious column name");
					return NULL;
				}
				matching_alias = cur_alias;
				column = t_column;
			}

		}
		cur_join = cur_join->next;
	} while(cur_join != start_join);

	if(column == NULL) {
		WARNING(CUSTOM_ERROR, "Unknown column");
		return NULL;
	}

	ret = (SqlColumnAlias*)malloc(sizeof(SqlColumnAlias));
	ret->column = column;
	PACK_SQL_STATEMENT(ret->table_alias, matching_alias, table_alias);

	return ret;
}
