/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_get_keys(LogicalPlan *plan) {
	LogicalPlan *select = plan;

	GET_LP(select, select, 0, LP_PROJECT);
	GET_LP(select, select, 1, LP_SELECT);
	GET_LP(select, select, 1, LP_CRITERIA);
	GET_LP(select, select, 0, LP_KEYS);
	assert(NULL != select); // All tables should have at least one key
	return select;
}
