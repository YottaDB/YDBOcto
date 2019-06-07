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

LogicalPlan *lp_get_output_key(LogicalPlan *plan) {
	LogicalPlan *t;

	plan = lp_drill_to_insert(plan);
	assert(plan->type == LP_INSERT);
	GET_LP(t, plan, 1, LP_OUTPUT);
	GET_LP(t, t, 0, LP_KEY);
	return t;
}
