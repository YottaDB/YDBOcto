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

int qualify_column_list(SqlColumnList *select_columns, SqlJoin *tables, SqlStatement *column_list_alias) {
	SqlColumnList *cur_column_list, *start_column_list;
	int result = 0;

	if(select_columns == NULL)
		return 0;

	cur_column_list = start_column_list = select_columns;
	do {
		result |= qualify_statement(cur_column_list->value, tables, column_list_alias);
		cur_column_list = cur_column_list->next;
	} while(cur_column_list != start_column_list);
	return result;
}
