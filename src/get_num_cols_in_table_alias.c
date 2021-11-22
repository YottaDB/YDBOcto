/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
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

/* Returns the number of columns in the input "table_alias" */
int get_num_cols_in_table_alias(SqlTableAlias *table_alias) {
	SqlColumnListAlias *start_cla, *cur_cla;
	int		    num_cols;

	num_cols = 0;
	UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
	cur_cla = start_cla;
	do {
		num_cols++;
		cur_cla = cur_cla->next;
	} while (cur_cla != start_cla);
	return num_cols;
}
