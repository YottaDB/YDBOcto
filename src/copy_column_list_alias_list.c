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

/* A copy function to create a copy of the given list.
 * When sql_stmt is non-NULL, column_alias from sql_stmt and keywords are additionally copied over to nodes of cla_copy.
 */
SqlColumnListAlias *copy_column_list_alias_list(SqlColumnListAlias *cla, SqlStatement *sql_stmt, SqlStatement *keywords) {
	SqlColumnListAlias *cla_copy, *cla_new, *cla_cur;

	cla_copy = NULL;
	cla_cur = cla;
	do {
		OCTO_CMALLOC_STRUCT(cla_new, SqlColumnListAlias);
		if (NULL == sql_stmt) {
			assert(NULL == cla_cur->duplicate_of_column);
			*cla_new = *cla_cur;
		} else {
			SqlColumnAlias *column_alias;
			SqlColumnList * cur;

			cla_new->alias = cla_cur->alias;
			cla_new->type = cla_cur->type;
			cla_new->keywords = keywords;
			OCTO_CMALLOC_STRUCT(cur, SqlColumnList);
			dqinit(cur);
			column_alias = get_column_alias_for_column_list_alias(cla_cur, sql_stmt);
			PACK_SQL_STATEMENT(cur->value, column_alias, column_alias);
			PACK_SQL_STATEMENT(cla_new->column_list, cur, column_list);
		}
		dqinit(cla_new);
		if (NULL == cla_copy) {
			cla_copy = cla_new;
		} else {
			dqappend(cla_copy, cla_new);
		}
		cla_cur = cla_cur->next;
	} while (cla_cur != cla);
	return cla_copy;
}
