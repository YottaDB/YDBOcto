/* Copyright (C) 2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
		GET_LP(lp_table_alias, table_join, 0, LP_TABLE);
		if(lp_table_alias->v.table_alias->unique_id == table_alias->unique_id)
			break;
		GET_LP(table_join, table_join, 1, LP_TABLE_JOIN);
	} while(table_join != NULL);
	assert(table_join != NULL);
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
		cur_lp_key->v.key->random_id = unique_id;
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

