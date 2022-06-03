/****************************************************************
 *								*
 * Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Finds the first physical plan (starting from "pplan" and going back parent plans as needed) whose keys match the input
 * "unique_id". Returns the matching physical plan. In addition, "*matching_key" is set to the matching key from the
 * returned physical plan in that case. If no physical plan is found, it returns NULL and "*matching_key" is set to NULL.
 */
PhysicalPlan *get_physical_plan_and_key_for_unique_id(PhysicalPlan *pplan, int unique_id, SqlKey **matching_key) {
	PhysicalPlan *cur_plan, *matching_plan;

	matching_plan = NULL;
	*matching_key = NULL;
	cur_plan = pplan;
	do {
		unsigned int  i;
		PhysicalPlan *next_plan;

		next_plan = cur_plan->parent_plan;
		for (i = 0; i < cur_plan->total_iter_keys; i++) {
			SqlKey *key;

			key = cur_plan->iterKeys[i];
			if (key->unique_id == unique_id) {
				assert((NULL == matching_plan) || (matching_plan == cur_plan));
				matching_plan = cur_plan;
				*matching_key = key;
				break;
			}
		}
		cur_plan = next_plan;
	} while (NULL != cur_plan);
	return matching_plan;
}
