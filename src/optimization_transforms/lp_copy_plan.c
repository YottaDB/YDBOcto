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

#include <stdlib.h>

#include "logical_plan.h"

LogicalPlan *lp_copy_plan(LogicalPlan *plan) {
	LogicalPlan *new_plan;

	if (NULL == plan)
		return NULL;
	/* Check a few plan types which do not require any copy. Return right away without a malloc/copy in that case. */
	switch (plan->type) {
	case LP_KEY:
		/* Don't see any reason why we will need to modify the SqlKey structure fields under a LP_KEY
		 * (and in turn keep separate copies for the source and target plans). Therefore, no need to
		 * do any copy even for the LP_KEY case.
		 */
	case LP_VALUE:
	case LP_TABLE:
	case LP_TABLE_DATA:
	case LP_ROW_VALUE:
	case LP_COLUMN:
	case LP_COLUMN_ALIAS:
	case LP_COLUMN_LIST_ALIAS:
	case LP_KEYWORDS:
	case LP_PIECE_NUMBER:
		return plan;
		break;
	default:
		break;
	}
	OCTO_CMALLOC_STRUCT(new_plan, LogicalPlan);
	*new_plan = *plan;
	if (LP_VIEW == plan->type) {
		// View definition doesn't need to be copied
		assert(new_plan->v.lp_default.operand[0] == plan->v.lp_default.operand[0]);
		new_plan->v.lp_default.operand[1] = lp_copy_plan(plan->v.lp_default.operand[1]);
	} else {
		new_plan->v.lp_default.operand[0] = lp_copy_plan(plan->v.lp_default.operand[0]);
		new_plan->v.lp_default.operand[1] = lp_copy_plan(plan->v.lp_default.operand[1]);
	}
	if (LP_TABLE_JOIN == plan->type) {
		new_plan->extra_detail.lp_table_join.join_on_condition
		    = lp_copy_plan(plan->extra_detail.lp_table_join.join_on_condition);
	}
	if (LP_SELECT_QUERY == new_plan->type) {
		/* After a new DNF copy of the LP_SELECT_QUERY logical plan has been created, check for any
		 * ORDER BY COLUMN NUM N usages in prior plan (in that case the Nth LP_COLUMN_LIST plan list under
		 * LP_ORDER_BY would point to the exact same Nth logical plan under the LP_COLUMN_LIST under
		 * LP_PROJECT). If present, that connection would have been severed by the "lp_copy_plan()" call as
		 * each LP_COLUMN_LIST under LP_ORDER_BY and LP_PROJECT would have been copied over to different
		 * memory. Re-establish the connection.
		 */
		LogicalPlan *output;

		GET_LP(output, new_plan, 1, LP_OUTPUT);
		if (NULL != output->v.lp_default.operand[1]) {
			LogicalPlan *order_by, *project, *select_column_list;

			GET_LP(order_by, output, 1, LP_ORDER_BY);
			GET_LP(project, new_plan, 0, LP_PROJECT);
			GET_LP(select_column_list, project, 0, LP_COLUMN_LIST);
			while (NULL != order_by) {
				if (order_by->extra_detail.lp_order_by.order_by_column_num) {
					/* This is an ORDER BY COLUMN NUM N case. Handle it. */
					LogicalPlan *nth_column_list;
					nth_column_list = lp_get_col_num_n_in_select_column_list(
					    select_column_list, order_by->extra_detail.lp_order_by.order_by_column_num);
					order_by->v.lp_default.operand[0] = nth_column_list;
				}
				order_by = order_by->v.lp_default.operand[1];
			}
		}
	}
	return new_plan;
}
