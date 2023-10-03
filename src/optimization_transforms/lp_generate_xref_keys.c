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
	LogicalPlan *root, *keys, *table_join, *lp_table_alias, *lp_output_key;
	int	     cur_key, max_key, unique_id;
	SqlColumn *  key_columns[MAX_KEY_COUNT], *column;
	SqlKey *     output_key;

	unique_id = table_alias->unique_id;
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	// Check if column is a computed column. If so, we cannot create a cross-reference on this.
	if (NULL != get_keyword_from_keywords(column->keywords->v.keyword, OPTIONAL_EXTRACT))
		return NULL;
	// Scan through and replace the table
	table_join = lp_get_table_join(plan);
	do {
		assert(LP_TABLE_VALUE != table_join->v.lp_default.operand[0]->type); /* Caller should have ensured this */
		if (table_join->v.lp_default.operand[0]->type == LP_TABLE) {
			GET_LP(lp_table_alias, table_join, 0, LP_TABLE);
			if (lp_table_alias->v.lp_table.table_alias->unique_id == table_alias->unique_id)
				break;
		}
		if (NULL != table_join->v.lp_default.operand[1]) {
			GET_LP(table_join, table_join, 1, LP_TABLE_JOIN);
		} else {
			table_join = NULL;
		}
	} while (NULL != table_join);
	if (NULL == table_join)
		return NULL;
	table_join->v.lp_default.operand[0] = lp_generate_xref_plan(table, column, unique_id);
	if (NULL == table_join->v.lp_default.operand[0])
		return NULL;
	lp_output_key = lp_get_output_key(table_join->v.lp_default.operand[0]);
	output_key = lp_output_key->v.lp_key.key;

	MALLOC_LP(keys, root, LP_KEYS);
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
	max_key = get_key_columns(table, key_columns);
	for (cur_key = 0; cur_key <= max_key; cur_key++) {
		keys->v.lp_default.operand[0]
		    = lp_alloc_key(table, key_columns[cur_key], unique_id, LP_KEY_ADVANCE, output_key, FALSE, NULL);
		if (cur_key != max_key) {
			MALLOC_LP_2ARGS(keys->v.lp_default.operand[1], LP_KEYS);
			GET_LP(keys, keys, 1, LP_KEYS);
		}
	}
	return root;
}
