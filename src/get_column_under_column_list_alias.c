/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* This function is mainly used to get the leaf SqlColumn from a SqlColumnListAlias tree if it represents
 * a simple column. This traversal is only needed when the SqlColumnListAlias is formed to represent a subquery column.
 * A subquery column will have multiple `SqlColumnListAlias -> SqlColumnList -> SqlColumnAlias` structures in that order under the
 * same column_list_alias_STATEMENT.
 * Input:
 *   cla - The SqlColumnListAlias to traverse
 * Return:
 *   SqlColumn - The SqlColumnListAlias represents a simple column and the leaf was returned
 *   NULL      - The SqlColumnListAlias has more complicated structure, so couldn't get the leaf column
 */
SqlColumn *get_column_under_column_list_alias(SqlColumnListAlias *cla) {
	if (NULL == cla) {
		assert(FALSE);
		return NULL;
	}

	SqlStatement *stmt = cla->column_list;
	for (; NULL != stmt;) {
		switch (stmt->type) {
		case column_list_alias_STATEMENT:;
			SqlColumnListAlias *cla;
			UNPACK_SQL_STATEMENT(cla, stmt, column_list_alias);
			stmt = cla->column_list;
			break;
		case column_list_STATEMENT:;
			SqlColumnList *cl;
			UNPACK_SQL_STATEMENT(cl, stmt, column_list);
			stmt = cl->value;
			break;
		case column_alias_STATEMENT:;
			SqlColumnAlias *ca;
			UNPACK_SQL_STATEMENT(ca, stmt, column_alias);
			stmt = ca->column;
			break;
		case column_STATEMENT:;
			SqlColumn *c;
			UNPACK_SQL_STATEMENT(c, stmt, column);
			return c;
		default:
			return NULL;
			break;
		}
	}
	return NULL;
}
