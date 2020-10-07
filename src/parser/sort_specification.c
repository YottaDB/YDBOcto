/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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
// When supported collate clause SqlStatement may be passed here.
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *ordering_specification) {
	SqlStatement *	    ret, *order_spec;
	SqlColumnListAlias *alias;

	SQL_COLUMN_LIST_ALIAS_STATEMENT(ret);
	UNPACK_SQL_STATEMENT(alias, ret, column_list_alias);
	alias->column_list = create_sql_column_list(sort_key, NULL, &sort_key->loc);
	// Add a keyword for ASC or DESC. Default to ASC if not explicitly specified.
	SQL_STATEMENT(order_spec, keyword_STATEMENT);
	OCTO_CMALLOC_STRUCT(order_spec->v.keyword, SqlOptionalKeyword);
	order_spec->v.keyword->keyword
	    = ((NULL == ordering_specification) || ((SqlStatement *)OPTIONAL_ASC == ordering_specification)) ? OPTIONAL_ASC
													     : OPTIONAL_DESC;
	order_spec->v.keyword->v = NULL;
	dqinit(order_spec->v.keyword);
	alias->keywords = order_spec;
	return ret;
}
