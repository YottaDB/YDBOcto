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

int lp_verify_structure_helper(LogicalPlan *plan, LPActionType exected);

/**
 * Verifies the given LP has a good structure; return TRUE if s'all good
 *  and FALSE otherwise
 */
int lp_verify_structure(LogicalPlan *plan) {
	LogicalPlan *cur;

	cur = plan;
	if(cur->type != LP_INSERT && cur->type != LP_SET_OPERATION)
		return FALSE;
	return TRUE;
}

int lp_verify_structure_helper(LogicalPlan *plan, LPActionType expected) {
	int ret = TRUE;
        // Cases where NULL is not allowed is enforced in the switch below
	if(plan == NULL)
		return TRUE;
	if(plan->type != expected)
		return FALSE;
	switch(expected) {
	case LP_INSERT:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEY);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_PROJECT);
		break;
	case LP_SET_OPERATION:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_INSERT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_OPERATION);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_INSERT)
			| lp_verify_structure_helper(plan->v.operand[1], LP_SET_OPERATION);;
		break;
	case LP_TABLE:
		// NULL is valid here, so just verify the type
		break;
	case LP_PROJECT:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_COLUMN_LIST);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_SELECT);
		break;
	case LP_COLUMN_LIST:
		// NULL valid here, so just verify type above
		break;
	case LP_SELECT:
		if(plan->v.operand[0] == NULL || plan->v.operand[1] == NULL)
			return FALSE;
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_TABLE);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_CRITERIA);
		break;
	case LP_CRITERIA:
		if(plan->v.operand[0] == NULL)
			return FALSE;
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEYS);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_WHERE);
		break;
	case LP_KEYS:
		if(plan->v.operand[0] == NULL)
			return FALSE;
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEY);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_KEY)
			| lp_verify_structure_helper(plan->v.operand[1], LP_KEYS);
		break;
	case LP_KEY:
		if(plan->v.operand[0] == NULL)
			return FALSE;
		// The second operator should always be NULL
		if(plan->v.operand[1] != NULL)
			return FALSE;
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEY_FIX)
			| lp_verify_structure_helper(plan->v.operand[0], LP_KEY_ADVANCE)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_UNION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_INTERSECT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_DIFFERENCE);
		break;
	case LP_KEY_FIX:
		if(plan->v.operand[0] == NULL)
			return FALSE;
		if(plan->v.operand[1] != NULL)
			return FALSE;
		if(plan->v.operand[0]->v.key == NULL)
			return FALSE;
		if(plan->v.operand[0]->v.key->value == NULL)
			return FALSE;
		break;
	case LP_KEY_ADVANCE:
		if(plan->v.operand[0] == NULL)
			return FALSE;
		if(plan->v.operand[1] != NULL)
			return FALSE;
		if(plan->v.operand[0]->v.key == NULL)
			return FALSE;
		if(plan->v.operand[0]->v.key->value != NULL)
			return FALSE;
		if(plan->v.operand[0]->v.key->column == NULL)
			return FALSE;
		break;
	case LP_SET_UNION:
	case LP_SET_INTERSECT:
	case LP_SET_DIFFERENCE:
		if(plan->v.operand[0] == NULL || plan->v.operand[1] == NULL)
			return FALSE;
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEY_FIX)
			| lp_verify_structure_helper(plan->v.operand[0], LP_KEY_ADVANCE)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_UNION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_INTERSECT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_DIFFERENCE);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_KEY_FIX)
			| lp_verify_structure_helper(plan->v.operand[1], LP_KEY_ADVANCE)
			| lp_verify_structure_helper(plan->v.operand[1], LP_SET_UNION)
			| lp_verify_structure_helper(plan->v.operand[1], LP_SET_INTERSECT)
			| lp_verify_structure_helper(plan->v.operand[1], LP_SET_DIFFERENCE);
		break;
	case LP_WHERE:
		if(plan->v.operand[0] == NULL || plan->v.operand[1] != NULL)
			return FALSE;
	        ret &= lp_verify_structure_helper(plan->v.operand[0], LP_ADDITION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SUBTRACTION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_DIVISION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_MULTIPLICATION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_CONCAT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_OR)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_AND)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_IS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_NOT_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_LESS_THAN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_GREATER_THAN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_LESS_THAN_OR_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_GREATER_THAN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_IN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_NOT_IN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_VALUE);
		break;
	case LP_ADDITION:
	case LP_SUBTRACTION:
	case LP_DIVISION:
	case LP_MULTIPLICATION:
	case LP_CONCAT:
	case LP_BOOLEAN_OR:
	case LP_BOOLEAN_AND:
	case LP_BOOLEAN_IS:
	case LP_BOOLEAN_EQUALS:
	case LP_BOOLEAN_NOT_EQUALS:
	case LP_BOOLEAN_LESS_THAN:
	case LP_BOOLEAN_GREATER_THAN:
	case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
	case LP_BOOLEAN_IN:
	case LP_BOOLEAN_NOT_IN:
	        ret &= lp_verify_structure_helper(plan->v.operand[0], LP_ADDITION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SUBTRACTION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_DIVISION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_MULTIPLICATION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_CONCAT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_OR)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_AND)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_IS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_NOT_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_LESS_THAN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_GREATER_THAN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_LESS_THAN_OR_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_GREATER_THAN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_IN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_BOOLEAN_NOT_IN)
			| lp_verify_structure_helper(plan->v.operand[0], LP_VALUE);
	        ret &= lp_verify_structure_helper(plan->v.operand[1], LP_ADDITION)
			| lp_verify_structure_helper(plan->v.operand[1], LP_SUBTRACTION)
			| lp_verify_structure_helper(plan->v.operand[1], LP_DIVISION)
			| lp_verify_structure_helper(plan->v.operand[1], LP_MULTIPLICATION)
			| lp_verify_structure_helper(plan->v.operand[1], LP_CONCAT)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_OR)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_AND)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_IS)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_NOT_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_LESS_THAN)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_GREATER_THAN)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_LESS_THAN_OR_EQUALS)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_GREATER_THAN)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_IN)
			| lp_verify_structure_helper(plan->v.operand[1], LP_BOOLEAN_NOT_IN)
			| lp_verify_structure_helper(plan->v.operand[1], LP_VALUE);
		break;
	default:
		// This is OK for now; we should fill things in as needed
		break;

	}
	return ret;
}
