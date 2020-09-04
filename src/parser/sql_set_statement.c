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

SqlStatement *sql_set_statement(SqlStatement *variable, SqlStatement *value, ParseContext *parse_context) {
	if (parse_context->is_extended_query) {
		parse_context->command_tag = set_STATEMENT;
	}
	SqlStatement *	 ret;
	SqlSetStatement *set;
	SQL_STATEMENT(ret, set_STATEMENT);
	MALLOC_STATEMENT(ret, set, SqlSetStatement);
	UNPACK_SQL_STATEMENT(set, ret, set);
	set->variable = variable;
	set->value = value;
	return ret;
}
