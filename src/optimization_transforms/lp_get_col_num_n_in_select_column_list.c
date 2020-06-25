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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/* Returns the 'n'th LP_COLUMN_LIST in the SELECT column list for a given plan */
LogicalPlan *lp_get_col_num_n_in_select_column_list(LogicalPlan *column_list, int n) {
	int num_cols;

	assert(LP_COLUMN_LIST == column_list->type);
	assert(NULL != column_list);
	num_cols = 0;
	do {
		num_cols++;
		if (num_cols == n) {
			break;
		}
		column_list = column_list->v.lp_default.operand[1];
	} while (NULL != column_list);
	assert(num_cols);
	return column_list;
}
