/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "logical_plan.h"
#include "physical_plan.h"

/* Given a physical plan corresponding to an UPDATE query, this function returns TRUE if there is at least one key column
 * specified in the SET clause OR if there is at least one cross reference key. Either of these conditions is an indication
 * to the caller to generate additional M code to ensure the same row does not get processed twice as part of the UPDATE.
 *
 * The code below is very similar to that in "src/physical/get_num_key_cols_in_set_clause.c".
 */
int is_update_keycol_or_xref(PhysicalPlan *pplan) {
	LogicalPlan *lp_update;
	unsigned int iter_key_index, total_keys;

	lp_update = pplan->lp_select_query;
	assert(LP_UPDATE == lp_update->type);
	total_keys = pplan->total_iter_keys;
	iter_key_index = 0;
	for (;;) {
		SqlKey *plan_key;

		plan_key = pplan->iterKeys[iter_key_index];
		if (plan_key->is_cross_reference_key) {
			return TRUE; /* Found a cross reference key */
		}
		LogicalPlan *lp_column_list;
		lp_column_list = lp_get_update_column_list(lp_update);

		do {
			LogicalPlan *lp_upd_col_value, *lp_column;
			SqlColumn   *cur_column;

			GET_LP(lp_upd_col_value, lp_column_list, 0, LP_UPD_COL_VALUE);
			GET_LP(lp_column, lp_upd_col_value, 0, LP_COLUMN);
			cur_column = lp_column->v.lp_column.column;
			if (cur_column == plan_key->column) {
				return TRUE; /* Found a key column that was specified in the SET clause of the UPDATE */
			}
			GET_LP_ALLOW_NULL(lp_column_list, lp_column_list, 1, LP_COLUMN_LIST);
		} while (NULL != lp_column_list);
		iter_key_index++;
		if (iter_key_index >= total_keys) {
			break;
		}
	}
	return FALSE; /* Found neither a cross reference key or a key column in the SET clause */
}
