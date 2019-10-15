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

int get_column_piece_number(SqlColumnAlias *alias, SqlTableAlias *table_alias) {
	SqlColumn		*column;
	SqlValue		*value;
	SqlColumnListAlias	*cur_cl_alias, *start_cl_alias, *t_cl_alias;
	char			*column_name1, *column_name2;
	int			part;

	if (column_STATEMENT == alias->column->type) {
		UNPACK_SQL_STATEMENT(column, alias->column, column);
		assert(column->column_number);
		return column->column_number;
	}
	UNPACK_SQL_STATEMENT(t_cl_alias, alias->column, column_list_alias);
	UNPACK_SQL_STATEMENT(value, t_cl_alias->alias, value);
	column_name1 = value->v.string_literal;
	UNPACK_SQL_STATEMENT(start_cl_alias, table_alias->column_list, column_list_alias);
	cur_cl_alias = start_cl_alias;
	part = 1;
	do {
		UNPACK_SQL_STATEMENT(value, cur_cl_alias->alias, value);
		column_name2 = value->v.string_literal;
		if (!strcmp(column_name1, column_name2))
			break;
		part++;
		cur_cl_alias = cur_cl_alias->next;
	} while(cur_cl_alias != start_cl_alias);
	assert((1 == part) || (cur_cl_alias != start_cl_alias));
	return part;
}
