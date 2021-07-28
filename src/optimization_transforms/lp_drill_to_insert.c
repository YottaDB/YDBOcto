/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#ifndef NDEBUG
	boolean_t    first_time = TRUE;
	LPActionType set_oper_type;
#endif

	assert((LP_SELECT_QUERY == cur_plan->type) || (LP_SET_OPERATION == cur_plan->type) || (LP_TABLE_VALUE == cur_plan->type));
	while (LP_SET_OPERATION == cur_plan->type) {
		// Fetch one of the output plans from this set
		assert(LP_SET_OPTION == cur_plan->v.lp_default.operand[0]->type);
		assert(LP_PLANS == cur_plan->v.lp_default.operand[1]->type);
#ifndef NDEBUG
		LogicalPlan *set_option;
		GET_LP(set_option, cur_plan, 0, LP_SET_OPTION);
		if (first_time) {
			set_oper_type = set_option->v.lp_default.operand[0]->type;
			first_time = FALSE;
		} else if (LP_SET_DNF == set_oper_type) {
			assert(LP_SET_DNF == set_option->v.lp_default.operand[0]->type);
		}
#endif
		cur_plan = cur_plan->v.lp_default.operand[1];
		cur_plan = cur_plan->v.lp_default.operand[0];
	}
	assert((LP_SELECT_QUERY == cur_plan->type) || (LP_TABLE_VALUE == cur_plan->type));
	return cur_plan;
}
