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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

// Returns the column number of the input cla in the input table_alias
// Searches for the cla in the table_alias SELECT column list (linked list)
int get_column_number_from_column_list_alias(SqlColumnListAlias *input_cla, SqlTableAlias *table_alias) {
	SqlColumnListAlias	*start_cla, *cur_cla;
	int			column_number;

	column_number = 1;
	UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
	cur_cla = start_cla;
	do {
		if (cur_cla == input_cla) {
			break;
		}
		column_number++;
		cur_cla = cur_cla->next;
		assert(cur_cla != start_cla);
	} while (cur_cla != start_cla);
	return column_number;
}
