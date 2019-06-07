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

int qualify_column_list_alias(SqlColumnListAlias *alias, SqlJoin *tables, SqlStatement *column_list_alias) {
	SqlColumnListAlias *cur_alias, *start_alias;
	SqlColumnList *column_list;
	int ret = 0;

	cur_alias = start_alias = alias;
	do {
		UNPACK_SQL_STATEMENT(column_list, cur_alias->column_list, column_list);
		ret |= qualify_column_list(column_list, tables, column_list_alias);
		cur_alias = cur_alias->next;
	} while(cur_alias != start_alias);
	return ret;
}
