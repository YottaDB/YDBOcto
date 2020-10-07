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

// Function invoked by the rule named "table_expression" in src/parser/select.y
SqlStatement *table_expression(SqlStatement *from, SqlStatement *where, SqlStatement *group_by, SqlStatement *having) {
	SqlStatement *ret;

	SQL_STATEMENT(ret, select_STATEMENT);
	MALLOC_STATEMENT(ret, select, SqlSelectStatement);
	ret->v.select->table_list = from;
	ret->v.select->where_expression = where;
	ret->v.select->group_by_expression = group_by;
	ret->v.select->having_expression = having;
	return ret;
}
