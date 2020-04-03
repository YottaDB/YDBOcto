/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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

boolean_t match_column_list_alias_in_select_column_list(SqlColumnListAlias *match_cla, SqlStatement *cla_stmt)
{
	SqlColumnListAlias	*start_cla, *cur_cla;
	boolean_t		match;

	UNPACK_SQL_STATEMENT(start_cla, cla_stmt, column_list_alias);
	cur_cla = start_cla;
	match = FALSE;
	do {
		if (match_sql_statement(match_cla->column_list, cur_cla->column_list)) {
			match = TRUE;
			break;
		}
		cur_cla = cur_cla->next;
	} while (cur_cla != start_cla);
	return match;
}
