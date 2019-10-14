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
	SqlSortSpecList		*sort_spec_list;

	assert(NULL == collate_clause);	/* COLLATE feature is currently not supported */
	SQL_STATEMENT(ret, sort_spec_list_STATEMENT);
	MALLOC_STATEMENT(ret, sort_spec_list, SqlSortSpecList);
	UNPACK_SQL_STATEMENT(sort_spec_list, ret, sort_spec_list);
	sort_spec_list->column_value = sort_key;
	// Add a keyword for ASC or DESC. Default to ASC if not explicitly specified.
	SQL_STATEMENT(order_spec, keyword_STATEMENT);
	OCTO_CMALLOC_STRUCT(order_spec->v.keyword, SqlOptionalKeyword);
	order_spec->v.keyword->keyword = ((NULL == ordering_specification)
							|| ((SqlStatement *)OPTIONAL_ASC == ordering_specification))
						? OPTIONAL_ASC
						: OPTIONAL_DESC;
	order_spec->v.keyword->v = NULL;
	dqinit(order_spec->v.keyword);
	sort_spec_list->sort_type = order_spec;
	dqinit(sort_spec_list);
	return ret;
}
