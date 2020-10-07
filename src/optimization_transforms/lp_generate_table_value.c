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
#include "logical_plan.h"

/* Generates a LP_TABLE_VALUE table plan corresponding to a VALUES() clause specification and returns it */
LogicalPlan *lp_generate_table_value(SqlStatement *stmt, boolean_t *caller_error_encountered) {
	SqlTableValue *	    table_value;
	SqlStatement *	    table_value_stmt;
	LogicalPlan *	    lp_table_value, *lp_table_data, *lp_row_value_next, *lp_table_key;
	SqlRowValue *	    row_value, *start_row_value;
	SqlColumnListAlias *cla;
	SqlTableAlias *	    table_alias;

	assert(table_alias_STATEMENT == stmt->type);
	table_alias = stmt->v.table_alias;
	table_value_stmt = table_alias->table;
	assert(table_value_STATEMENT == table_value_stmt->type);
	MALLOC_LP_2ARGS(lp_table_value, LP_TABLE_VALUE);
	lp_table_value->extra_detail.lp_insert.root_table_alias = table_alias;
	UNPACK_SQL_STATEMENT(cla, table_alias->column_list, column_list_alias);
	MALLOC_LP(lp_table_data, lp_table_value->v.lp_default.operand[0], LP_TABLE_DATA);
	lp_table_data->v.lp_default.operand[0] = lp_column_list_to_lp(cla, caller_error_encountered);
	lp_row_value_next = lp_table_data;
	UNPACK_SQL_STATEMENT(table_value, table_value_stmt, table_value);
	UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
	start_row_value = row_value;
	do {
		SqlColumnList *column_list;
		LogicalPlan *  lp_row_value;

		MALLOC_LP(lp_row_value, lp_row_value_next->v.lp_default.operand[1], LP_ROW_VALUE);
		UNPACK_SQL_STATEMENT(column_list, row_value->value_list, column_list);
		*caller_error_encountered |= lp_generate_column_list(&lp_row_value->v.lp_default.operand[0], stmt, column_list);
		lp_row_value_next = lp_row_value;
		row_value = row_value->next;
	} while (row_value != start_row_value);
	MALLOC_LP(lp_table_data, lp_table_value->v.lp_default.operand[1], LP_OUTPUT);
	MALLOC_LP(lp_table_key, lp_table_data->v.lp_default.operand[0], LP_KEY);
	OCTO_CMALLOC_STRUCT(lp_table_key->v.lp_key.key, SqlKey);
	lp_table_key->v.lp_key.key->unique_id = table_alias->unique_id;
	lp_table_key->v.lp_key.key->type = LP_KEY_ADVANCE;
	return lp_table_value;
}
