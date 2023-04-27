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
	case LP_TABLE_VALUE:
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
	new_plan->v.lp_default.operand[0] = lp_copy_plan(plan->v.lp_default.operand[0]);
	new_plan->v.lp_default.operand[1] = lp_copy_plan(plan->v.lp_default.operand[1]);
	if (LP_TABLE_JOIN == plan->type) {
		new_plan->extra_detail.lp_table_join.join_on_condition
		    = lp_copy_plan(plan->extra_detail.lp_table_join.join_on_condition);
	}
	return new_plan;
}
