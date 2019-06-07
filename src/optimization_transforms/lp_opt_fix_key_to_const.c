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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/**
 * Fixes the key to a specific value
 *
 * If the key is already fixed, changes it to a UNION
 *
 * @returns TRUE (1) if the key was fixed, or FALSE if it couldn't be done
 */
int lp_opt_fix_key_to_const(LogicalPlan *root, SqlKey *key, LogicalPlan *value) {
	// We can't fix keys that are already fixed, or keys that are part of an
	//  cross reference iteration
	UNUSED(root);
	if(key->type == LP_KEY_ADVANCE && key->cross_reference_output_key == NULL) {
		key->value = lp_copy_plan(value);
		key->type = LP_KEY_FIX;
	} else {
		return FALSE;
	}
	return TRUE;
}
