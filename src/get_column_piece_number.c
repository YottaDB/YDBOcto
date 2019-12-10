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

#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

int get_column_piece_number(SqlColumnAlias *column_alias, SqlTableAlias *table_alias) {
	SqlColumn		*column;
	SqlColumnListAlias	*cur_cl_alias, *start_cl_alias;
	int			part;

	if (column_STATEMENT == column_alias->column->type) {
		UNPACK_SQL_STATEMENT(column, column_alias->column, column);
		assert(column->column_number);
		return column->column_number;
	}
	UNPACK_SQL_STATEMENT(start_cl_alias, table_alias->column_list, column_list_alias);
	cur_cl_alias = start_cl_alias;
	part = 1;
	do {
		if (column_alias == cur_cl_alias->outer_query_column_alias) {
			break;
		}
		part++;
		cur_cl_alias = cur_cl_alias->next;
	} while (cur_cl_alias != start_cl_alias);
	assert((1 == part) || (cur_cl_alias != start_cl_alias));
	return part;
}
