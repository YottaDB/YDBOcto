/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Given a LP_UPDATE logical plan, this function returns the corresponding LP_COLUMN_LIST plan storing
 * the list of column names in the SET clause of the UPDATE command (i.e. the columns that are being updated).
 */
LogicalPlan *lp_get_update_column_list(LogicalPlan *lp_update) {
	LogicalPlan *lp_update_options;
	LogicalPlan *lp_column_list;

	assert(LP_UPDATE == lp_update->type);
	GET_LP(lp_update_options, lp_update, 1, LP_UPDATE_OPTIONS);
	GET_LP(lp_column_list, lp_update_options, 0, LP_COLUMN_LIST);
	return lp_column_list;
}
