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

#include <stdlib.h>

#include "logical_plan.h"

LogicalPlan *lp_join_where(LogicalPlan *where1, LogicalPlan *where2) {
	LogicalPlan *new_plan, *w;
	if (where1 == NULL)
		return where2;
	if (where2 == NULL)
		return where1;
	if (where1->v.lp_default.operand[0] == NULL)
		return where2;
	if (where2->v.lp_default.operand[0] == NULL)
		return where1;
	OCTO_CMALLOC_STRUCT(new_plan, LogicalPlan);
	new_plan->type = LP_BOOLEAN_AND;
	assert(where1->type == LP_WHERE);
	w = where1->v.lp_default.operand[0];
	assert(where2->type == LP_WHERE);
	new_plan->v.lp_default.operand[0] = w;
	new_plan->v.lp_default.operand[1] = where2->v.lp_default.operand[0];
	where1->v.lp_default.operand[0] = new_plan;
	return where1;
}
