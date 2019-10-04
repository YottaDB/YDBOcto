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
	SqlColumn		*cur_column, *start_column;
	SqlOptionalKeyword	*keyword, *column_keywords;
	SqlTable		*table;
	int			column_number;

	UNPACK_SQL_STATEMENT(table, table_statement, table);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	column_number = 0;
	do {
		column_number++;
		cur_column->table = table_statement;
		// Also a good time to assign each column a PIECE number if it doesn't have one
		keyword = get_keyword(cur_column, OPTIONAL_PIECE);
		if (NULL == keyword) {
			keyword	= add_optional_piece_keyword_to_sql_column(column_number);
			UNPACK_SQL_STATEMENT(column_keywords, cur_column->keywords, keyword);
			dqappend(column_keywords, keyword);
		}
		cur_column->column_number = column_number;
		cur_column = cur_column->next;
	} while(cur_column != start_column);
}
