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
	LogicalPlan *new_plan;
	if(plan == NULL)
		return NULL;
	new_plan = (LogicalPlan *)octo_cmalloc(memory_chunks, sizeof(LogicalPlan));
	*new_plan = *plan;
	/// TODO: should this also clone tables and what not?
	switch(plan->type) {
	case LP_VALUE:
	case LP_TABLE:
	case LP_KEY:
	case LP_COLUMN_ALIAS:
		break;
	default:
		new_plan->v.operand[0] = lp_copy_plan(plan->v.operand[0]);
		new_plan->v.operand[1] = lp_copy_plan(plan->v.operand[1]);
	}
	return new_plan;
}
