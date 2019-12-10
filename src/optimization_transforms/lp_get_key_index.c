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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

int lp_get_key_index(LogicalPlan *plan, LogicalPlan *lp_column_alias) {
  	SqlColumnAlias *column_alias;
	SqlColumn *column;
	SqlValue *key_table_name, *search_table_name, *key_column_name, *search_column_name;
	SqlTableAlias *table_alias;
	SqlTable *table;
	SqlColumnListAlias *cl_alias;
	SqlKey *key;
	int key_id, search_id, cur_key_index;
	LogicalPlan *cur_key, *lp_key;

	column_alias = lp_column_alias->v.lp_column_alias.column_alias;
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias, table_alias);
	search_id = table_alias->unique_id;
	if(table_alias->table->type == table_STATEMENT) {
		UNPACK_SQL_STATEMENT(table, table_alias->table, table);
		UNPACK_SQL_STATEMENT(search_table_name, table->tableName, value);
	} else {
		assert(table_alias->table->type == select_STATEMENT);
		UNPACK_SQL_STATEMENT(search_table_name, table_alias->alias, value);
	}

	if(column_alias->column->type == column_STATEMENT) {
		UNPACK_SQL_STATEMENT(column, column_alias->column, column);
		UNPACK_SQL_STATEMENT(search_column_name, column->columnName, value);
	} else {
		UNPACK_SQL_STATEMENT(cl_alias, column_alias->column, column_list_alias);
		UNPACK_SQL_STATEMENT(search_column_name, cl_alias->alias, value);
	}

	cur_key = lp_get_keys(plan);
	cur_key_index = 0;

	do {
		GET_LP(lp_key, cur_key, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		key_id = key->unique_id;
		/// TODO: the only way something has a name of NULL is if it's an output key
		// Which means we're looking for the key in a derived table; we don't currently
		// support this
		if(key->table == NULL) {
			assert(key->column == NULL);
			return -2;
		}
		do {
			if(key_id != search_id)
				break;
			UNPACK_SQL_STATEMENT(key_table_name, key->table->tableName, value);
			if(strcmp(search_table_name->v.string_literal, key_table_name->v.string_literal) != 0)
				break;
			UNPACK_SQL_STATEMENT(key_column_name, key->column->columnName, value);
			if(strcmp(search_column_name->v.string_literal, key_column_name->v.string_literal) != 0)
				break;
			return cur_key_index;
		} while(TRUE);
		cur_key = cur_key->v.lp_default.operand[1];
		cur_key_index++;
	} while(cur_key != NULL);
	return -1;
}
