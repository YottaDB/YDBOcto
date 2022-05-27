/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/
#include "physical_plan.h"

/* The following function is used by `tmpl_column_reference_common` and `tmpl_column_list_combine`
 * to check if the passed `unique_id` is part of `pplan`'s `iterKeys`.
 * Returns TRUE if the unique_id is found in pplan->total_iter_keys else FALSE.
 */
boolean_t is_unique_id_a_key_of_pplan(PhysicalPlan *pplan, int unique_id) {
	for (unsigned int i = 0; i < pplan->total_iter_keys; i++) {
		SqlKey *key;
		key = pplan->iterKeys[i];
		if (key->unique_id == unique_id) {
			return TRUE;
		}
	}
	return FALSE;
}
