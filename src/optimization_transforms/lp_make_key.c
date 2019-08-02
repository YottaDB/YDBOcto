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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_make_key(SqlColumnAlias *column_alias) {
	LogicalPlan *ret;
	SqlColumn *column;
	SqlTableAlias *table_alias;
	SqlTable *table;

	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias, table_alias);
	UNPACK_SQL_STATEMENT(table, table_alias->table, table);

	MALLOC_LP_2ARGS(ret, LP_KEY);
	OCTO_CMALLOC_STRUCT(ret->v.key, SqlKey);
	memset(ret->v.key, 0, sizeof(SqlKey));
	ret->v.key->column = column;
	ret->v.key->key_num = -1;
	ret->v.key->unique_id = table_alias->unique_id;
	ret->v.key->table = table;
	ret->v.key->type = LP_KEY_ADVANCE;
	return ret;
}
