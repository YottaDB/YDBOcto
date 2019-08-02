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
	LogicalPlan *column_list, *ret_column_list = NULL;
	LogicalPlan *where;
	LogicalPlan *column_list_alias;
	SqlColumnListAlias *cur_column_list, *start_column_list;
	SqlColumnList *t_column_list;
	assert(list != NULL);

	MALLOC_LP_2ARGS(column_list, LP_COLUMN_LIST);

	cur_column_list = start_column_list = list;
	do {
		MALLOC_LP(where, column_list->v.operand[0], LP_WHERE);
		/// TODO: handle the absence of prev
		UNPACK_SQL_STATEMENT(t_column_list, cur_column_list->column_list, column_list);
		where->v.operand[0] = lp_generate_where(t_column_list->value, plan_id);
		MALLOC_LP(column_list_alias, where->v.operand[1], LP_COLUMN_LIST_ALIAS);
		// When we do this copy, we only want a single CLA; this prevents the copy from
		//   grabbing more
		OCTO_CMALLOC_STRUCT(column_list_alias->v.column_list_alias, SqlColumnListAlias);
		dqinit(column_list_alias->v.column_list_alias);
		column_list_alias->v.column_list_alias->alias = cur_column_list->alias;
		column_list_alias->v.column_list_alias->type = cur_column_list->type;
		cur_column_list = cur_column_list->next;
		if(ret_column_list == NULL)
			ret_column_list = column_list;
		if(cur_column_list != start_column_list) {
			MALLOC_LP_2ARGS(column_list->v.operand[1], LP_COLUMN_LIST);
			column_list = column_list->v.operand[1];
		}
	} while(cur_column_list != start_column_list);


	return ret_column_list;
}
