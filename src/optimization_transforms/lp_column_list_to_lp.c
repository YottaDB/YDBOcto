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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_column_list_to_lp(SqlColumnListAlias *list, int *plan_id) {
	LogicalPlan		*column_list, *ret_column_list = NULL;
	LogicalPlan		*column_list_alias;
	LogicalPlan		*where;
	SqlColumnList		*t_column_list;
	SqlColumnListAlias	*cur_cla, *start_cla;
	SqlStatement		*column_stmt;
	boolean_t		null_return_seen = FALSE;

	assert(NULL != list);
	MALLOC_LP_2ARGS(column_list, LP_COLUMN_LIST);

	cur_cla = start_cla = list;
	do {
		MALLOC_LP(where, column_list->v.lp_default.operand[0], LP_WHERE);
		column_stmt = cur_cla->column_list;
		UNPACK_SQL_STATEMENT(t_column_list, column_stmt, column_list);
		LP_GENERATE_WHERE(t_column_list->value, plan_id, column_stmt, where->v.lp_default.operand[0], null_return_seen);
		MALLOC_LP(column_list_alias, where->v.lp_default.operand[1], LP_COLUMN_LIST_ALIAS);
		column_list_alias->v.lp_column_list_alias.column_list_alias = cur_cla;
		cur_cla = cur_cla->next;
		if (NULL == ret_column_list)
			ret_column_list = column_list;
		if (cur_cla != start_cla) {
			MALLOC_LP_2ARGS(column_list->v.lp_default.operand[1], LP_COLUMN_LIST);
			column_list = column_list->v.lp_default.operand[1];
		}
	} while (cur_cla != start_cla);
	return (null_return_seen ? NULL : ret_column_list);
}
