/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "logical_plan.h"
#include "physical_plan.h"

/* Allocate and initialize (a few fields) a physical plan. Returns the allocated physical plan. */
PhysicalPlan *allocate_physical_plan(LogicalPlan *plan, PhysicalPlanOptions *plan_options, PhysicalPlanOptions *orig_plan_options) {
	PhysicalPlan *pplan;

	OCTO_CMALLOC_STRUCT(pplan, PhysicalPlan);
	assert((LP_SELECT_QUERY == plan->type) || (LP_TABLE_VALUE == plan->type) || (LP_INSERT_INTO == plan->type)
	       || (LP_DELETE_FROM == plan->type));
	plan->extra_detail.lp_select_query.physical_plan = pplan;
	assert(NULL == pplan->prev);
	pplan->lp_select_query = plan;
	pplan->parent_plan = orig_plan_options->parent;
	plan_options->parent = pplan;
	pplan->next = *plan_options->last_plan;
	if (NULL != *plan_options->last_plan) {
		(*plan_options->last_plan)->prev = pplan;
	}
	*plan_options->last_plan = pplan;
	return pplan;
}
