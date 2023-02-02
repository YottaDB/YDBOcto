/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by the rule named "query_specification" in src/parser/select.y
 * Responsible to form the table_alias_STATEMENT holding all passed arguments.
 */
SqlStatement *query_specification(OptionalKeyword set_quantifier, SqlStatement *select_list, SqlStatement *table_expression,
				  SqlStatement *sort_specification_list, int *plan_id) {
	SqlStatement *	    ret, *quantifier;
	SqlTableAlias *	    this_table_alias;
	SqlSelectStatement *select;

	SQL_STATEMENT(ret, table_alias_STATEMENT);
	MALLOC_STATEMENT(ret, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(this_table_alias, ret, table_alias);
	SQL_VALUE_STATEMENT(this_table_alias->alias, NUL_VALUE, "");
	assert(select_STATEMENT == table_expression->type);
	this_table_alias->table = table_expression;
	this_table_alias->unique_id = (*plan_id)++;
	assert(column_list_alias_STATEMENT == select_list->type);
	this_table_alias->column_list = select_list;
	UNPACK_SQL_STATEMENT(select, table_expression, select);
	select->select_list = select_list;
	MALLOC_KEYWORD_STMT(quantifier, set_quantifier);
	quantifier->v.keyword->v = NULL;
	select->optional_words = quantifier;
	select->order_by_expression = sort_specification_list;
	return ret;
}
