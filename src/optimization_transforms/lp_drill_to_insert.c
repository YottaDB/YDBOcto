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

LogicalPlan *lp_drill_to_insert(LogicalPlan *plan) {
	LogicalPlan *cur_plan = plan;

	while (LP_INSERT != cur_plan->type)
	{
		assert(cur_plan->type == LP_SET_OPERATION);
		// Fetch one of the output plans from this set
		assert(LP_SET_OPTION == cur_plan->v.operand[0]->type);
		assert(LP_PLANS == cur_plan->v.operand[1]->type);
		cur_plan = cur_plan->v.operand[1];
		cur_plan = cur_plan->v.operand[0];
	}
	return cur_plan;
}
