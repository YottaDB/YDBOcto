/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

LogicalPlan *lp_get_output_key(LogicalPlan *plan) {
	LogicalPlan *t;

	assert((LP_INSERT == plan->type) || (LP_SET_OPERATION == plan->type));
	if (LP_SET_OPERATION == plan->type) {
		LogicalPlan *set_option;
		LPActionType set_oper_type;

		GET_LP(set_option, plan, 0, LP_SET_OPTION);
		set_oper_type = set_option->v.lp_default.operand[0]->type;
		assert((LP_SET_UNION == set_oper_type) || (LP_SET_UNION_ALL == set_oper_type) || (LP_SET_DNF == set_oper_type)
		       || (LP_SET_EXCEPT == set_oper_type) || (LP_SET_EXCEPT_ALL == set_oper_type)
		       || (LP_SET_INTERSECT == set_oper_type) || (LP_SET_INTERSECT_ALL == set_oper_type));
		// For LP_SET_DNF, each of the sub plans should have the same output key, so we can grab from either
		// For all other set operations, we should grab the output key from under the LP_SET_OPTION plan.
		if (LP_SET_DNF == set_oper_type)
			plan = lp_drill_to_insert(plan);
		else
			plan = set_option;
	}
	GET_LP(t, plan, 1, LP_OUTPUT);
	GET_LP(t, t, 0, LP_KEY);
	return t;
}
