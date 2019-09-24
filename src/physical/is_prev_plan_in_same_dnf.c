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

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

// Returns TRUE if "plan" and "plan->prev" are expanded plans from the same Disjunctive Normal Form
// (expansion happens in "lp_make_normal_disjunctive_form.c").
boolean_t	is_prev_plan_in_same_dnf(PhysicalPlan *plan)
{
	boolean_t	prev_plan_is_in_same_dnf;
	PhysicalPlan	*prev_plan;

	prev_plan = plan->prev;
	assert((NULL == prev_plan) || (NULL != prev_plan->outputKey));
	assert(NULL != plan->outputKey);
	assert((NULL == prev_plan) || prev_plan->total_iter_keys);
	if ((NULL == prev_plan) || !prev_plan->total_iter_keys || !plan->total_iter_keys)
		return FALSE;
	prev_plan_is_in_same_dnf = (plan->iterKeys[0]->unique_id == prev_plan->iterKeys[0]->unique_id);
	assert(!prev_plan_is_in_same_dnf || plan->emit_duplication_check);
	return prev_plan_is_in_same_dnf;
}
