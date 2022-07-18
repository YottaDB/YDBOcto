/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Given a column name and a SqlTableAlias, this function determines the corresponding
 * column alias for that column and returns that.
 */
SqlColumnAlias *get_column_alias_from_column_name(char *columnName, SqlTableAlias *table_alias) {
	SqlTable *	    table;
	SqlColumn *	    tbl_col;
	SqlColumnListAlias *cla;
	SqlColumnList *	    cl;
	SqlColumnAlias *    ret;

	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	tbl_col = find_column(columnName, table);
	assert(NULL != tbl_col);
	cla = get_column_list_alias_n_from_table_alias(table_alias, tbl_col->column_number);
	UNPACK_SQL_STATEMENT(cl, cla->column_list, column_list);
	UNPACK_SQL_STATEMENT(ret, cl->value, column_alias);
	return ret;
}
