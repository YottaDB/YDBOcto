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

#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

int get_column_piece_number(SqlColumnAlias *column_alias, SqlTableAlias *table_alias) {
	SqlColumnListAlias *cur_cl_alias, *start_cl_alias;
	int		    piece_number;

	if (table_value_STATEMENT == table_alias->table->type) {
		SqlColumn *column;

		assert(column_STATEMENT == column_alias->column->type);
		UNPACK_SQL_STATEMENT(column, column_alias->column, column);
		assert(0 < column->column_number);
		return column->column_number;
	}
	assert(column_STATEMENT != column_alias->column->type);
	UNPACK_SQL_STATEMENT(start_cl_alias, table_alias->column_list, column_list_alias);
	cur_cl_alias = start_cl_alias;
	piece_number = 1;
	do {
		if (column_alias == cur_cl_alias->outer_query_column_alias) {
			break;
		}
		piece_number++;
		cur_cl_alias = cur_cl_alias->next;
	} while (cur_cl_alias != start_cl_alias);
	assert((1 == piece_number) || (cur_cl_alias != start_cl_alias));
	return piece_number;
}
