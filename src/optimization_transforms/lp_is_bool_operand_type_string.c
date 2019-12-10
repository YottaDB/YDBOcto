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

boolean_t lp_is_bool_operand_type_string(LogicalPlan *plan) {
	boolean_t	ret, loop_done;
	LogicalPlan	*cur_plan;
	SqlColumnAlias	*column_alias;

	assert((LP_BOOLEAN_LESS_THAN == plan->type)
		|| (LP_BOOLEAN_GREATER_THAN == plan->type)
		|| (LP_BOOLEAN_LESS_THAN_OR_EQUALS == plan->type)
		|| (LP_BOOLEAN_GREATER_THAN_OR_EQUALS == plan->type)
		|| (LP_BOOLEAN_ANY_LESS_THAN == plan->type)
		|| (LP_BOOLEAN_ANY_GREATER_THAN == plan->type)
		|| (LP_BOOLEAN_ANY_LESS_THAN_OR_EQUALS == plan->type)
		|| (LP_BOOLEAN_ANY_GREATER_THAN_OR_EQUALS == plan->type)
		|| (LP_BOOLEAN_ANY_EQUALS == plan->type)
		|| (LP_BOOLEAN_ANY_NOT_EQUALS == plan->type)
		|| (LP_BOOLEAN_ALL_LESS_THAN == plan->type)
		|| (LP_BOOLEAN_ALL_GREATER_THAN == plan->type)
		|| (LP_BOOLEAN_ALL_LESS_THAN_OR_EQUALS == plan->type)
		|| (LP_BOOLEAN_ALL_GREATER_THAN_OR_EQUALS == plan->type)
		|| (LP_BOOLEAN_ALL_EQUALS == plan->type)
		|| (LP_BOOLEAN_ALL_NOT_EQUALS == plan->type));
	/* We assume all values in this expression have the same type, which should be true due to the matching of types
	 * further up the stack (in `populate_data_type`). Traverse down the left side of the logical plan tree until we get
	 * to a plan node which has only a left child (right hand child is NULL) OR stop traversing if we end up with
	 * determine the type of that node
	 */
	cur_plan = plan;
	ret = FALSE;
	for ( loop_done = FALSE; !loop_done; ) {
		switch(cur_plan->type) {
		case LP_VALUE:
			if (STRING_LITERAL == cur_plan->v.lp_value.value->type) {
				ret = TRUE;
			}
			loop_done = TRUE;
			break;
		case LP_COLUMN_ALIAS:
		case LP_DERIVED_COLUMN:
			column_alias = ((LP_COLUMN_ALIAS == cur_plan->type)
						? cur_plan->v.lp_column_alias.column_alias
						: cur_plan->extra_detail.lp_derived_column.subquery_column_alias);
			if (column_alias->column->type == column_STATEMENT) {
				if (CHARACTER_STRING_TYPE == column_alias->column->v.column->type) {
					ret = TRUE;
				}
			} else {
				assert(column_alias->column->type == column_list_alias_STATEMENT);
				if (STRING_LITERAL == column_alias->column->v.column_list_alias->type) {
					ret = TRUE;
				}
			}
			loop_done = TRUE;
			break;
		case LP_COLUMN_LIST_ALIAS:
		case LP_TABLE:
		case LP_KEY:
		case LP_KEYWORDS:
		case LP_PIECE_NUMBER:
			/* These cases should never show up inside a boolean expression. Hence the below assert. */
			assert(FALSE);
			break;
		default:
			/* Due to the above switch/case blocks, if we reach here, it means this plan type is guaranteed to
			 * have `cur_plan->v.lp_default` usable. This relies on the current layout of the `v` member in the
			 * `LogicalPlan` structure and that none of the LP_* possibilities in the "default:" case here
			 * can possibly have a `v` member usable other than `lp_default`. Any changes to the `v` union
			 * layout might need to be reflected here (LOGICAL_PLAN_KEEP_IN_SYNC).
			 */
			cur_plan = cur_plan->v.lp_default.operand[0];
			assert(NULL != cur_plan);
			break;
		}
	}
	return ret;
}
