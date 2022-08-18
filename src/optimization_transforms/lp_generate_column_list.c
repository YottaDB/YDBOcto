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
#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

boolean_t lp_generate_column_list(LogicalPlan **ret, SqlStatement *root_stmt, SqlColumnList *start_columns) {
	LogicalPlan *  prev, *next;
	SqlColumnList *cur_cl;
	boolean_t      error_encountered = FALSE;

	assert(NULL != ret);
	assert(NULL != start_columns);
	cur_cl = start_columns;

	// Use an LP_COLUMN_LIST to store the LP_VALUEs used
	MALLOC_LP_2ARGS(next, LP_COLUMN_LIST);
	*ret = next;

	// There must be at least one column
	do {
		assert(NULL != next);
		LP_GENERATE_WHERE(cur_cl->value, root_stmt, next->v.lp_default.operand[0], error_encountered);
		prev = next;
		cur_cl = cur_cl->next;
		MALLOC_LP_2ARGS(next, LP_COLUMN_LIST);
		prev->v.lp_default.operand[1] = next;
	} while (start_columns != cur_cl);
	// We allocated an extra column at the end.
	// We don't need to deallocate it since memory chunks are freed all at once at the end of the query.
	prev->v.lp_default.operand[1] = NULL;
	return error_encountered;
}
