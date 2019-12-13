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
	boolean_t	ret;
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
	// We assume all values in this expression have the same type, which should be true
	// due to the matching of types further up the stack
	// Delve down the left side until we get to a leaf node (right hand side is NULL) and
	// determine the type of that node
	cur_plan = plan;
	while (NULL != cur_plan->v.operand[1]) {
		cur_plan = cur_plan->v.operand[0];
		assert(NULL != cur_plan);
		if (LP_DERIVED_COLUMN == cur_plan->type) {
			break;
		}
	}
	ret = FALSE;
	switch(cur_plan->type) {
	case LP_VALUE:
		if (STRING_LITERAL == cur_plan->v.value->type) {
			ret = TRUE;
		}
		break;
	case LP_COLUMN_ALIAS:
	case LP_DERIVED_COLUMN:
		column_alias = ((LP_COLUMN_ALIAS == cur_plan->type) ? cur_plan->v.column_alias : cur_plan->subquery_column_alias);
		if (column_alias->column->type == column_STATEMENT) {
			if (column_alias->column->v.column->type == CHARACTER_STRING_TYPE) {
				ret = TRUE;
			}
		} else {
			assert(column_alias->column->type == column_list_alias_STATEMENT);
			if (STRING_LITERAL == column_alias->column->v.column_list_alias->type) {
				ret = TRUE;
			}
		}
		break;
	default:
		assert(FALSE);
		break;
	}
	return ret;
}
