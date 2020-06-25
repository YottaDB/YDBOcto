/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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

/* Finds the physical plan with an output key matching the passed in `unique_id` and returns it.
 * The input parameter `pplan` is used to find the first physical plan in the linked list and traverse through it.
 */
PhysicalPlan *get_physical_plan_from_unique_id(PhysicalPlan *pplan, int unique_id) {
	PhysicalPlan *cur_plan;
	int	      output_id;

	cur_plan = pplan;
	// Walk the plans back to the first
	while (NULL != cur_plan->prev) {
		cur_plan = cur_plan->prev;
	}
	for (; NULL != cur_plan; cur_plan = cur_plan->next) {
		if (NULL != cur_plan->set_oper_list) {
			/* The physical plan has SET operations. In that case, the output key is stored in the SET oper list */
			output_id = cur_plan->set_oper_list->output_id;
		} else {
			/* The output key is stored in the physical plan itself */
			output_id = cur_plan->outputKey->unique_id;
		}
		if (unique_id == output_id) {
			return cur_plan;
		}
	}
	assert(FALSE);
	return NULL;
}
