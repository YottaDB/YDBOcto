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

	SQL_COLUMN_LIST_ALIAS_STATEMENT(ret);
	UNPACK_SQL_STATEMENT(alias, ret, column_list_alias);
	alias->column_list = create_sql_column_list(derived_column_expression, NULL, yloc);
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
	return ret;
}
