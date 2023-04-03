/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
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
	int	      output_id = 0;

	assert(0 != unique_id);
	cur_plan = pplan;
	// Walk the plans back to the first
	while (NULL != cur_plan->prev) {
		cur_plan = cur_plan->prev;
	}
	for (; NULL != cur_plan; cur_plan = cur_plan->next) {
		/* First check if cur_plan is a view, if so its possible for one of viewKeys[i]->unique_id to match unique_id. Since
		 * a view's pplan can be a set_operation or select query we might miss going through this block if we don't do it
		 * first.
		 */
		if (0 != cur_plan->view_total_iter_keys) {
			/* The physical plan corresponds to a VIEW.
			 * The unique_id's of Logical plan structures which map to a single physical plan are stored in
			 * viewKeys[]. See if the unique_id is present in the list. If so return the physical plan holding
			 * the list with the matching key.
			 */
			for (unsigned int i = 0; i < cur_plan->view_total_iter_keys; i++) {
				SqlKey *key;

				key = cur_plan->viewKeys[i];
				if (key->unique_id == unique_id) {
					return cur_plan;
				}
			}
		} else if (NULL != cur_plan->set_oper_list) {
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
