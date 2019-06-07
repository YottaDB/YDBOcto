/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

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
		// We need to create a new SqlStatement here because only the table is propegated
		//  long term; the SqlStatement gets reused by the compiler
		PACK_SQL_STATEMENT(cur_column->table, table, table);
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
