/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

SqlOptionalKeyword *add_optional_piece_keyword_to_sql_column(int column_number)
{
	SqlOptionalKeyword	*keyword;
	SqlStatement		*stmt;
	char			*malloc_space;
	char			buffer[32];	// 32 bytes as it is more than enough to store string form of an integer
	int			len;

	SQL_STATEMENT(stmt, keyword_STATEMENT);
	MALLOC_STATEMENT(stmt, keyword, SqlOptionalKeyword);
	keyword = stmt->v.keyword;
	keyword->keyword = OPTIONAL_PIECE;
	snprintf(buffer, sizeof(buffer), "%d", column_number);
	len = strlen(buffer);
	malloc_space = octo_cmalloc(memory_chunks, len+1);
	strncpy(malloc_space, buffer, len+1);
	SQL_STATEMENT(keyword->v, value_STATEMENT);
	MALLOC_STATEMENT(keyword->v, value, SqlValue);
	keyword->v->v.value->type = NUMERIC_LITERAL;
	keyword->v->v.value->v.string_literal = malloc_space;
	dqinit(keyword);
	return keyword;
}
