/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "octo.h"
#include "logical_plan.h"

/* Given a LP_UPDATE logical plan and a search column, this function checks if the column is found in the
 * SET clause of the UPDATE command. Returns TRUE if yes, and FALSE otherwise.
 */
boolean_t lp_is_col_in_update_column_list(LogicalPlan *lp_update, SqlColumn *srch_column) {
	LogicalPlan *lp_column_list;

	assert(LP_UPDATE == lp_update->type);
	lp_column_list = lp_get_update_column_list(lp_update);
	do {
		LogicalPlan *lp_upd_col_value;
		GET_LP(lp_upd_col_value, lp_column_list, 0, LP_UPD_COL_VALUE);

		LogicalPlan *lp_column;
		GET_LP(lp_column, lp_upd_col_value, 0, LP_COLUMN);

		SqlColumn *cur_column;
		cur_column = lp_column->v.lp_column.column;
		assert(cur_column->table->v.create_table == srch_column->table->v.create_table);
		if (cur_column == srch_column) {
			return TRUE;
		}
		GET_LP_ALLOW_NULL(lp_column_list, lp_column_list, 1, LP_COLUMN_LIST);
	} while (NULL != lp_column_list);
	return FALSE;
}
