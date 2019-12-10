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

void init_parent_table_alias(SqlStatement *table_alias_stmt, SqlTableAlias *parent_table_alias) {
	SqlTableAlias		*table_alias;
	SqlSelectStatement	*select;
	SqlJoin			*start_join, *cur_join;

	assert(table_alias_STATEMENT == table_alias_stmt->type);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
	UNPACK_SQL_STATEMENT(select, table_alias->table, select);
	UNPACK_SQL_STATEMENT(start_join, select->table_list, join);
	cur_join = start_join;
	do {
		SqlStatement	*join_table_alias_stmt;
		SqlTableAlias	*join_table_alias;

		join_table_alias_stmt = cur_join->value;
		UNPACK_SQL_STATEMENT(join_table_alias, join_table_alias_stmt, table_alias);
		if (set_operation_STATEMENT == join_table_alias_stmt->type) {
			SqlSetOperation		*set_opr;

			UNPACK_SQL_STATEMENT(set_opr, join_table_alias_stmt, set_operation);
			init_parent_table_alias(set_opr->operand[0], parent_table_alias);
			init_parent_table_alias(set_opr->operand[1], parent_table_alias);
		} else {
			if (NULL == join_table_alias->parent_table_alias) {
				join_table_alias->parent_table_alias = parent_table_alias;
			} else {
				assert(join_table_alias->parent_table_alias == parent_table_alias);
			}
		}
		cur_join = cur_join->next;
	} while (cur_join != start_join);
	return;
}
