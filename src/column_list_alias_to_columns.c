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

/**
 * Creates a list of columns given a column_list_alias; useful for passing
 *  ownership up a level
 */
SqlColumn *column_list_alias_to_columns(SqlTableAlias *table_alias) {
	SqlColumnListAlias *cla_start, *cla_cur;
	SqlColumn *start_column = NULL, *cur_column, *t_column;

	if(table_alias->column_list == NULL)
		return NULL;
	UNPACK_SQL_STATEMENT(cla_start, table_alias->column_list, column_list_alias);
	cla_cur = cla_start;
	do {
		cur_column = (SqlColumn*)malloc(sizeof(SqlColumn));
		memset(cur_column, 0, sizeof(SqlColumn));
		cur_column->table = table_alias->table;
		if(cla_cur != NULL) {
			cur_column->columnName = cla_cur->alias;
		}
		SQL_STATEMENT(cur_column->keywords, keyword_STATEMENT);
		MALLOC_STATEMENT(cur_column->keywords, keyword, SqlOptionalKeyword);
		dqinit(cur_column->keywords->v.keyword);
		switch(cla_cur->type) {
		case UNKNOWN_SqlValueType:
		case NUMBER_LITERAL:
			cur_column->type = INTEGER_TYPE;
			break;
		case STRING_LITERAL:
			cur_column->type = CHARACTER_STRING_TYPE;
			break;
		default:
			FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "");
			break;
		}
		dqinit(cur_column);
		if(start_column == NULL) {
			start_column = cur_column;
		} else {
			dqinsert(cur_column, start_column, t_column);
		}
		cla_cur = cla_cur->next;
	} while(cla_start != cla_cur);
	return start_column;
}
