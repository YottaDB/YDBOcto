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

// Function invoked by the rule named "derived_column" in src/parser/select.y
SqlStatement *derived_column(SqlStatement *derived_column_expression, SqlStatement *column_name, struct YYLTYPE *yloc) {
	SqlColumnListAlias *alias;
	SqlStatement *	    ret;
	SqlColumnList *	    column_list;

	SQL_STATEMENT(ret, column_list_alias_STATEMENT);
	MALLOC_STATEMENT(ret, column_list_alias, SqlColumnListAlias);
	UNPACK_SQL_STATEMENT(alias, ret, column_list_alias);
	SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
	dqinit(alias);
	MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
	UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
	dqinit(column_list);
	column_list->value = derived_column_expression;
	if (NULL == column_name) {
		alias->alias = find_column_alias_name(derived_column_expression);
		if (NULL == alias->alias) {
			SQL_STATEMENT(alias->alias, value_STATEMENT);
			MALLOC_STATEMENT(alias->alias, value, SqlValue);
			alias->alias->v.value->type = STRING_LITERAL;
			alias->alias->v.value->v.string_literal = octo_cmalloc(memory_chunks, strlen("???") + 2);
			strcpy(alias->alias->v.value->v.string_literal, "???");
		}
		alias->user_specified_alias = FALSE;
	} else {
		alias->alias = column_name;
		alias->user_specified_alias = TRUE;
	}
	alias->column_list->loc = *yloc;
	return ret;
}
