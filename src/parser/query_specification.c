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
SqlStatement *query_specification(OptionalKeyword set_quantifier, SqlStatement *select_list,
					SqlStatement *table_expression, SqlStatement *sort_specification_list, int *plan_id)
{
	SqlStatement		*ret, *quantifier;
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
		SqlColumnListAlias	*cla_alias = NULL, *t_cla_alias;
		SqlJoin			*join, *cur_join, *start_join;
		SqlTableAlias		*table_alias;

		UNPACK_SQL_STATEMENT(join, select->table_list, join);
		start_join = cur_join = join;
		do
		{
			sql_stmt = cur_join->value;
			sql_stmt = drill_to_table_alias(sql_stmt);
			UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
			if (NULL != table_alias->column_list) {
				SqlColumnListAlias	*cla_start, *cla_cur, *cla_new;
				SqlColumnList		*cur;
				SqlColumnAlias		*column_alias;

				UNPACK_SQL_STATEMENT(cla_start, table_alias->column_list, column_list_alias);
				cla_cur = cla_start;
				do {
					OCTO_CMALLOC_STRUCT(cla_new, SqlColumnListAlias);
					cla_new->alias = cla_cur->alias;
					cla_new->type = cla_cur->type;
					OCTO_CMALLOC_STRUCT(cur, SqlColumnList);
					dqinit(cur);
					column_alias = get_column_alias_for_column_list_alias(cla_cur, sql_stmt);
					PACK_SQL_STATEMENT(cur->value, column_alias, column_alias);
					PACK_SQL_STATEMENT(cla_new->column_list, cur, column_list);
					dqinit(cla_new);
					if (NULL == cla_alias) {
						cla_alias = cla_new;
					} else {
						dqappend(cla_alias, cla_new);
					}
					cla_cur = cla_cur->next;
				} while (cla_start != cla_cur);
			}
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		/* Copy location of ASTERISK (noted down in ASTERISK rule in "src/parser/select.y")
		 * for potential error reporting (with line/column context) in "populate_data_type.c".
		 * Do this in all the created column list aliases.
		 */
		t_cla_alias = cla_alias;
		if (NULL != t_cla_alias) {
			do {
				t_cla_alias->column_list->loc = select_list->loc;
				t_cla_alias = t_cla_alias->next;
			} while (t_cla_alias != cla_alias);
		}
		select_list->v.column_list_alias = cla_alias;
	}
	SQL_STATEMENT(quantifier, keyword_STATEMENT);
	OCTO_CMALLOC_STRUCT(quantifier->v.keyword, SqlOptionalKeyword);
	quantifier->v.keyword->keyword = set_quantifier;
	quantifier->v.keyword->v = NULL;
	dqinit(quantifier->v.keyword);
	select->optional_words = quantifier;
	select->order_by_expression = sort_specification_list;
	return ret;
}
