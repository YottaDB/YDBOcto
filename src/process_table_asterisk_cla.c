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
#include <string.h>

/*
 * This function is invoked by qualify_statement.c, column_list_alias_STATEMENT case, for processing TABLE.ASTERISK usage in
 * Select column list, GROUP BY and Order By list.
 * It finds TABLE.ASTERISK node in `specification_list` and replaces it with nodes correponding to all columns of the table.
 * SqlColumnAlias for TABLE.ASTERISK node is already qualified with the correct table_alias_stmt. We just iterate through its
 * column_list to retrieve all the columns required by table.*. `c_cla_head`, `c_cla_cur and `specification_list` list pointers are
 * updated after modification to list.
 */
void process_table_asterisk_cla(SqlStatement *specification_list, SqlColumnListAlias **c_cla_cur, SqlColumnListAlias **c_cla_head) {
	SqlColumnListAlias *cla_cur, *cla_head, *result, *cla;
	SqlColumnAlias *    column_alias;
	SqlStatement *	    cur_table_alias_stmt;
	SqlTableAlias *	    cur_table_alias;
	SqlColumnList *	    cl_cur;

	cla_cur = *c_cla_cur;
	cla_head = *c_cla_head;
	assert(NULL != cla_cur);
	result = NULL;
	UNPACK_SQL_STATEMENT(cl_cur, cla_cur->column_list, column_list);
	UNPACK_SQL_STATEMENT(column_alias, cl_cur->value, column_alias);
	cur_table_alias_stmt = column_alias->table_alias_stmt;
	assert(NULL != cur_table_alias_stmt);
	UNPACK_SQL_STATEMENT(cur_table_alias, cur_table_alias_stmt, table_alias);
	/* Update the list with new nodes */
	UNPACK_SQL_STATEMENT(cla, cur_table_alias->column_list, column_list_alias);
	result = copy_column_list_alias_list(cla, cur_table_alias_stmt, cla_cur->keywords);
	REPLACE_COLUMNLISTALIAS(cla_cur, result, cla_head, specification_list);
	*c_cla_cur = cla_cur;
	*c_cla_head = cla_head;
}
