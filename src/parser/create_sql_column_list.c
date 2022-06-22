/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
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

// Function invoked by various rules in src/parser.y
SqlStatement *create_sql_column_list(SqlStatement *elem, SqlStatement *tail, YYLTYPE *llocp) {
	SqlColumnList *column_list, *cl_tail;
	SqlStatement * ret;

	SQL_STATEMENT(ret, column_list_STATEMENT);
	MALLOC_STATEMENT(ret, column_list, SqlColumnList);
	UNPACK_SQL_STATEMENT(column_list, ret, column_list);
	if (NULL != llocp) {
		ret->loc = *llocp;
	}
	column_list->value = elem;
	assert(0 == column_list->qualify_extract_function_cycle);
	dqinit(column_list);
	if (NULL != tail) {
		UNPACK_SQL_STATEMENT(cl_tail, tail, column_list);
		dqappend(column_list, cl_tail);
	}
	return ret;
}
