/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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
#include "logical_plan.h"

int lp_get_tablejoin_unique_id(LogicalPlan *plan) {
	LogicalPlan *oper0, *cur_lp_key;
	int	     unique_id;

	assert(LP_TABLE_JOIN == plan->type);
	oper0 = plan->v.lp_default.operand[0];
	switch (oper0->type) {
	case LP_SET_OPERATION:
	case LP_INSERT:
		cur_lp_key = lp_get_output_key(oper0);
		unique_id = cur_lp_key->v.lp_key.key->unique_id;
		break;
	default:
		assert((LP_TABLE == oper0->type) || (LP_TABLE_VALUE == oper0->type));
		unique_id = ((LP_TABLE == oper0->type) ? oper0->v.lp_table.table_alias->unique_id
						       : oper0->extra_detail.lp_insert.root_table_alias->unique_id);
		break;
	}
	return unique_id;
}
