/****************************************************************
 *								*
 * Copyright (c) 2020-2026 YottaDB LLC and/or its subsidiaries.	*
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
 * "*matching_key" is a key that holds the value of a key column: not a cross reference key, unless it is the cross reference
 * key of a key column (YDBOcto#660), which takes that column's place. It is the only key of a table with one key column
 * whose cross reference is walked (e.g. for an ORDER BY).
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
				if (!key->is_cross_reference_key) {
					*matching_key = key;
					break;
				}
				if (IS_KEY_COLUMN_XREF(key) && (NULL == *matching_key)) {
					*matching_key = key; /* Used if the table has no other key (see above) */
				}
			}
		}
		cur_plan = next_plan;
	} while (NULL != cur_plan);
	assert((NULL == matching_plan) || (NULL != matching_key));
	return matching_plan;
}
