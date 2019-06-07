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

void lp_insert_key(LogicalPlan *plan, LogicalPlan *key) {
	LogicalPlan *cur_keys, *t;

	cur_keys = lp_get_keys(plan);

	// Drill down to the last item
	if(cur_keys->v.operand[0] != NULL) {
		while(cur_keys->v.operand[1] != NULL) {
			cur_keys = cur_keys->v.operand[1];
		}
		t = MALLOC_LP(cur_keys->v.operand[1], LP_KEYS);
		t->v.operand[0] = key;
	} else {
		cur_keys->v.operand[0] = key;
	}
}
