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
	// First verify that the key unique_id is the same in plan and prev_plan.
	prev_plan_is_in_same_dnf = (plan->iterKeys[0]->unique_id == prev_plan->iterKeys[0]->unique_id);
	// This is usually good enough. In rare cases though, it is possible both plan and prev_plan have
	// the same unique_id but they are not plans from the same DNF. For example, if they are the same
	// sub-query that got expanded into different sides of a DNF expansion (e.g. below query from #362)
	//	SELECT 1 FROM t1 WHERE (1=1 OR 1=1) AND EXISTS(SELECT 1 FROM t1 AS x WHERE 1=t1.b);
	// Hence the additional check for "emit_duplication_check" below which gets set only for DNF expanded plans.
	// If that also turns out to be TRUE, then we are guaranteed plan and prev_plan are part of same DNF.
	// In the above example query, the EXISTS sub-query that gets copied over to multiple parts of the DNF will
	// have "emit_duplication_check" FALSE so this additional check will help return an accurate value.
	prev_plan_is_in_same_dnf &= plan->emit_duplication_check && prev_plan->emit_duplication_check;
	return prev_plan_is_in_same_dnf;
}
