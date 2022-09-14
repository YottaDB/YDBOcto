/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Given a physical plan corresponding to an UPDATE query, this function returns the number of key columns
 * specified in the SET clause.
 *
 * The code below is very similar to that in "src/physical/is_update_keycol_or_xref.c".
 *
 * This function/file is currently used only in one assert (i.e. Debug build only) and so is not used in Release builds
 * but is not removed in case it is found necessary in the future.
 */
int get_num_key_cols_in_set_clause(PhysicalPlan *pplan) {
	LogicalPlan *lp_update;
	unsigned int iter_key_index, total_keys;
	int	     num_key_cols_in_set_clause;

	lp_update = pplan->lp_select_query;
	assert(LP_UPDATE == lp_update->type);
	total_keys = pplan->total_iter_keys;
	num_key_cols_in_set_clause = 0;
	iter_key_index = 0;
	for (;;) {
		SqlKey *plan_key;

		plan_key = pplan->iterKeys[iter_key_index];
		if (!plan_key->is_cross_reference_key) {
			LogicalPlan *lp_column_list;
			lp_column_list = lp_get_update_column_list(lp_update);

			do {
				LogicalPlan *lp_upd_col_value, *lp_column;
				SqlColumn *  cur_column;

				GET_LP(lp_upd_col_value, lp_column_list, 0, LP_UPD_COL_VALUE);
				GET_LP(lp_column, lp_upd_col_value, 0, LP_COLUMN);
				cur_column = lp_column->v.lp_column.column;
				if (cur_column == plan_key->column) {
					/* This key column was specified in the SET clause. */
					num_key_cols_in_set_clause++;
					break;
				}
				lp_column_list = lp_column_list->v.lp_default.operand[1];
				assert((NULL == lp_column_list) || (LP_COLUMN_LIST == lp_column_list->type));
			} while (NULL != lp_column_list);
		}
		iter_key_index++;
		if (iter_key_index >= total_keys) {
			break;
		}
	}
	return num_key_cols_in_set_clause;
}
