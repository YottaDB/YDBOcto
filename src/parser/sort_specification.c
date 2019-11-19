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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

// Function invoked by the rule named "sort_specification" in src/parser/parser.y
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *collate_clause, SqlStatement *ordering_specification)
{
	SqlStatement		*ret, *order_spec;
	SqlColumnListAlias	*alias;
	SqlColumnList		*column_list;

	assert(NULL == collate_clause);	/* COLLATE feature is currently not supported */
	SQL_STATEMENT(ret, column_list_alias_STATEMENT);
	MALLOC_STATEMENT(ret, column_list_alias, SqlColumnListAlias);
	UNPACK_SQL_STATEMENT(alias, ret, column_list_alias);
	SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
	MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
	dqinit(alias);
	UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
	dqinit(column_list);
	column_list->value = sort_key;
	alias->column_list->loc = sort_key->loc;	// Cannot use "yyloc" here so passing it from parser through sort_key->loc
	// Add a keyword for ASC or DESC. Default to ASC if not explicitly specified.
	SQL_STATEMENT(order_spec, keyword_STATEMENT);
	OCTO_CMALLOC_STRUCT(order_spec->v.keyword, SqlOptionalKeyword);
	order_spec->v.keyword->keyword = ((NULL == ordering_specification)
							|| ((SqlStatement *)OPTIONAL_ASC == ordering_specification))
						? OPTIONAL_ASC
						: OPTIONAL_DESC;
	order_spec->v.keyword->v = NULL;
	dqinit(order_spec->v.keyword);
	alias->keywords = order_spec;
	return ret;
}
