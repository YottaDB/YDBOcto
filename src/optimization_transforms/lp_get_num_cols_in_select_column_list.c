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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/* Finds the number of columns in the SELECT column list for a given plan */
int lp_get_num_cols_in_select_column_list(LogicalPlan *plan) {
	int		num_cols;
	LogicalPlan	*column_list;

	assert((LP_INSERT == plan->type) || (LP_SET_OPERATION == plan->type));
	if (LP_SET_OPERATION == plan->type) {
		plan = lp_drill_to_insert(plan);
		assert(LP_INSERT == plan->type);
	}
	column_list = lp_get_projection_columns(plan);
	assert(NULL != column_list);
	num_cols = 0;
	do {
		num_cols++;
		column_list = column_list->v.lp_default.operand[1];
	} while (NULL != column_list);
	return num_cols;
}
