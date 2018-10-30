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

void assign_table_to_columns(SqlStatement *table_statement) {
	SqlColumn *cur_column, *start_column;
	SqlTable *table;
	SqlOptionalKeyword *keyword, *t_keyword, *column_keywords;
	SqlStatement *stmt;
	char buffer[MAX_STR_CONST], *malloc_space;
	int i, len;

	UNPACK_SQL_STATEMENT(table, table_statement, table);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);

	cur_column = start_column;
	i = 0;

	do {
		i++;
		cur_column->table = table_statement;
		// Also a good time to assign each column a PIECE number if it doesn't have one
		keyword = get_keyword(cur_column, OPTIONAL_PIECE);
		if(keyword == NULL) {
			SQL_STATEMENT(stmt, keyword_STATEMENT);
			MALLOC_STATEMENT(stmt, keyword, SqlOptionalKeyword);
			keyword = stmt->v.keyword;
			keyword->keyword = OPTIONAL_PIECE;
			snprintf(buffer, MAX_STR_CONST, "%d", i);
			len = strlen(buffer);
			malloc_space = malloc(len+1);
			strncpy(malloc_space, buffer, len+1);
			SQL_STATEMENT(keyword->v, value_STATEMENT);
			MALLOC_STATEMENT(keyword->v, value, SqlValue);
			keyword->v->v.value->type = STRING_LITERAL;
			keyword->v->v.value->v.string_literal = malloc_space;
			PACK_SQL_STATEMENT(stmt, keyword, keyword);
			dqinit(keyword);
			UNPACK_SQL_STATEMENT(column_keywords, cur_column->keywords, keyword);
			dqinsert(column_keywords, keyword, t_keyword);
		}
		cur_column = cur_column->next;
	} while(cur_column != start_column);
}
