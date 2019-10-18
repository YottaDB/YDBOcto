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

// Function invoked by the rule named "query_specification" in src/parser/select.y
SqlStatement *query_specification(SqlStatement *set_quantifier, SqlStatement *select_list,
					SqlStatement *table_expression, SqlStatement *sort_specification_list, int *plan_id)
{
	SqlStatement		*ret;
	SqlTableAlias		*this_table_alias;
	SqlValue		*value;
	SqlSelectStatement	*select;

	SQL_STATEMENT(ret, table_alias_STATEMENT);
	MALLOC_STATEMENT(ret, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(this_table_alias, ret, table_alias);
	SQL_STATEMENT(this_table_alias->alias, value_STATEMENT);
	MALLOC_STATEMENT(this_table_alias->alias, value, SqlValue);
	UNPACK_SQL_STATEMENT(value, this_table_alias->alias, value);
	value->type = NUL_VALUE;
	value->v.string_literal = "";
	assert(select_STATEMENT == table_expression->type);
	this_table_alias->table = table_expression;
	this_table_alias->unique_id = (*plan_id)++;
	assert(column_list_alias_STATEMENT == select_list->type);
	this_table_alias->column_list = select_list;
	UNPACK_SQL_STATEMENT(select, table_expression, select);
	select->select_list = select_list;
	// If the select list is empty, we need all columns from the joins in
	//  the order in which they are mentioned
	if (NULL == select_list->v.column_list_alias)
	{
		SqlStatement		*sql_stmt;
		SqlColumn		*t_column;
		SqlColumnListAlias	*cl_alias = NULL, *t_cl_alias;
		SqlJoin			*join, *cur_join, *start_join;
		SqlTableAlias		*table_alias;

		UNPACK_SQL_STATEMENT(join, select->table_list, join);
		start_join = cur_join = join;
		do
		{
			sql_stmt = cur_join->value;
			sql_stmt = drill_to_table_alias(sql_stmt);
			UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
			t_column = column_list_alias_to_columns(table_alias);
			t_cl_alias = columns_to_column_list_alias(t_column, table_alias);
			if (NULL == cl_alias)
				cl_alias = t_cl_alias;
			else
				dqappend(cl_alias, t_cl_alias);
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		/* Copy location of ASTERISK (noted down in ASTERISK rule in "src/parser/select.y")
		 * for potential error reporting (with line/column context) in "populate_data_type.c".
		 * Do this in all the created column list aliases.
		 */
		t_cl_alias = cl_alias;
		if (NULL != t_cl_alias) {
			do {
				t_cl_alias->column_list->loc = select_list->loc;
				t_cl_alias = t_cl_alias->next;
			} while (t_cl_alias != cl_alias);
		}
		select_list->v.column_list_alias = cl_alias;
	}
	select->optional_words = set_quantifier;
	select->order_expression = sort_specification_list;
	return ret;
}
