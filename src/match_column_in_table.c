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


SqlStatement *match_column_in_table(SqlTableAlias *table_alias, char *column_name, int column_name_len) {
	SqlColumnListAlias *cur_column_list, *start_column_list;
	SqlValue *value;
	SqlStatement *ret = NULL;
	int value_len;

	// If there is no column list for this table alias, we won't match anything
	if(table_alias->column_list == NULL)
		return NULL;
	UNPACK_SQL_STATEMENT(start_column_list, table_alias->column_list, column_list_alias);
	cur_column_list = start_column_list;
	do {
		if(cur_column_list->alias != NULL) {
			UNPACK_SQL_STATEMENT(value, cur_column_list->alias, value);
			value_len = strlen(value->v.string_literal);
			if(value_len == column_name_len && memcmp(value->v.string_literal, column_name, column_name_len) == 0) {
				PACK_SQL_STATEMENT(ret, cur_column_list, column_list_alias);
				break;
			}
		}
		cur_column_list = cur_column_list->next;
	} while(cur_column_list != start_column_list);

	return ret;
}
