/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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

SqlColumn *find_column(char *column_name, SqlTable *table) {
	SqlColumn *cur_column, *start_column;
	SqlValue * value;

	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		if (NULL != cur_column->columnName) {
			UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
			if (!strcmp(column_name, value->v.reference)) {
				return cur_column;
			}
		}
		/* else: It is a table-level constraint. Not a user-visible column. Skip it. */
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	return NULL;
}
