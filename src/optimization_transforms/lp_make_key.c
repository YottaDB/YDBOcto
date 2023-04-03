/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_make_key(SqlColumnAlias *column_alias) {
	LogicalPlan *  ret;
	SqlColumn *    column;
	SqlTable *     table;
	SqlTableAlias *table_alias;

	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	ret = lp_alloc_key(table, column, table_alias->unique_id, LP_KEY_ADVANCE, NULL, FALSE, NULL);
	return ret;
}
