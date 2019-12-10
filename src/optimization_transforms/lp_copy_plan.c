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

#include <stdlib.h>

#include "logical_plan.h"

LogicalPlan *lp_copy_plan(LogicalPlan *plan) {
	LogicalPlan	*new_plan;

	if (NULL == plan)
		return NULL;
	OCTO_CMALLOC_STRUCT(new_plan, LogicalPlan);
	*new_plan = *plan;
	switch(plan->type) {
	case LP_KEY:
		/* Copy SqlStatements which is definitely needed for keys (as these are modified later
		 * and need to be maintained separately for the source and target plans).
		 */
		new_plan->v.lp_key.key = lp_copy_key(plan->v.lp_key.key);
		break;
	case LP_VALUE:
	case LP_TABLE:
	case LP_COLUMN_ALIAS:
	case LP_COLUMN_LIST_ALIAS:
	case LP_KEYWORDS:
	case LP_PIECE_NUMBER:
		break;
	default:
		new_plan->v.lp_default.operand[0] = lp_copy_plan(plan->v.lp_default.operand[0]);
		new_plan->v.lp_default.operand[1] = lp_copy_plan(plan->v.lp_default.operand[1]);
		if (LP_TABLE_JOIN == plan->type) {
			new_plan->extra_detail.lp_table_join.join_on_condition
				= lp_copy_plan(plan->extra_detail.lp_table_join.join_on_condition);
		}
		break;
	}
	return new_plan;
}
