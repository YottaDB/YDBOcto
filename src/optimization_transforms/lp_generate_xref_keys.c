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

/**
 * Returns the keys corresponding to the cross reference for column in table, and updates
 * the LP_TABLE_JOIN of plan to include the plan which needs to be execute to generate the cross
 * reference
 */
LogicalPlan *lp_generate_xref_keys(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias) {
	LogicalPlan *root, *keys, *cur_lp_key, *table_join, *lp_table_alias, *lp_output_key;
	int cur_key, max_key, unique_id;
	SqlColumn *key_columns[MAX_KEY_COUNT], *column;
	SqlKey *output_key;

	unique_id = table_alias->unique_id;
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	// Scan through and replace the table
	table_join = lp_get_table_join(plan);
	do {
		if(table_join->v.operand[0]->type == LP_TABLE) {
			GET_LP(lp_table_alias, table_join, 0, LP_TABLE);
			if(lp_table_alias->v.table_alias->unique_id == table_alias->unique_id)
				break;
		}
		if(table_join->v.operand[1] != NULL) {
			GET_LP(table_join, table_join, 1, LP_TABLE_JOIN);
		} else {
			table_join = NULL;
		}
	} while(table_join != NULL);
	if(table_join == NULL)
		return NULL;
	/// TODO: free the old table
	table_join->v.operand[0] = lp_generate_xref_plan(plan, table, column, unique_id);
	lp_output_key = lp_get_output_key(table_join->v.operand[0]);
	output_key = lp_output_key->v.key;
	output_key->cross_reference_column_alias = column_alias;


	keys = MALLOC_LP(root, LP_KEYS);
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);
	for(cur_key = 0; cur_key <= max_key; cur_key++) {
		cur_lp_key = MALLOC_LP(keys->v.operand[0], LP_KEY);
		cur_lp_key->v.key = (SqlKey*)malloc(sizeof(SqlKey));
		memset(cur_lp_key->v.key, 0, sizeof(SqlKey));
		cur_lp_key->v.key->column = key_columns[cur_key];
		cur_lp_key->v.key->key_num = cur_key;
		cur_lp_key->v.key->unique_id = unique_id;
		cur_lp_key->v.key->table = table;
		cur_lp_key->v.key->type = LP_KEY_ADVANCE;
		cur_lp_key->v.key->cross_reference_output_key = output_key;
		if(cur_key != max_key) {
			MALLOC_LP(keys->v.operand[1], LP_KEYS);
			keys = keys->v.operand[1];
		}
	}
	// Replace references in the original plan
	//lp_replace_derived_table_references(plan, plan, table_alias);

	return root;
}

