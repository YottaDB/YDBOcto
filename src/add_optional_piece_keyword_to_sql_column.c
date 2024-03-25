/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

SqlOptionalKeyword *add_optional_piece_keyword_to_sql_column(int column_number) {
	SqlOptionalKeyword *keyword;
	SqlStatement	   *stmt;
	char		   *malloc_space;
	char		    buffer[32]; // 32 bytes as it is more than enough to store string form of an integer
	int		    len;

	MALLOC_KEYWORD_STMT(stmt, OPTIONAL_PIECE);
	keyword = stmt->v.keyword;
	snprintf(buffer, sizeof(buffer), "%d", column_number);
	len = strlen(buffer);
	malloc_space = octo_cmalloc(memory_chunks, len + 1);
	strncpy(malloc_space, buffer, len + 1);
	SQL_VALUE_STATEMENT(keyword->v, INTEGER_LITERAL, malloc_space);
	return keyword;
}
