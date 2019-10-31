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

int lp_verify_structure_helper(LogicalPlan *plan, LPActionType expected);

/* Verifies the given LP has a good structure; return TRUE if it is all good and FALSE otherwise */
int lp_verify_structure(LogicalPlan *plan) {
	if ((LP_INSERT != plan->type) && (LP_SET_OPERATION != plan->type))
	{
		assert(FALSE);
		return FALSE;
	}
	return lp_verify_structure_helper(plan, plan->type);
}

int lp_verify_structure_helper(LogicalPlan *plan, LPActionType expected) {
	int		i, ret = TRUE;

        // Cases where NULL is not allowed is enforced in the switch below
	if (NULL == plan)
		return TRUE;
	if (plan->type != expected)
		return FALSE;
	switch (expected) {
	case LP_INSERT:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_PROJECT);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_OUTPUT);
		break;
	case LP_OUTPUT:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEY);
		break;
	case LP_SET_OPERATION:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_SET_OPTION);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_PLANS);
		break;
	case LP_SET_OPTION:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_SET_UNION)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_UNION_ALL)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_EXCEPT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_EXCEPT_ALL)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_INTERSECT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_INTERSECT_ALL)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_DNF);
		assert((LP_SET_DNF != plan->v.operand[0]->type) || (NULL == plan->v.operand[1]));
		assert((LP_SET_DNF == plan->v.operand[0]->type) || (NULL != plan->v.operand[1]));
		if (NULL != plan->v.operand[1]) {
			ret &= lp_verify_structure_helper(plan->v.operand[1], LP_OUTPUT);
		}
		break;
	case LP_PLANS:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_INSERT)
			| lp_verify_structure_helper(plan->v.operand[0], LP_SET_OPERATION);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_INSERT)
			| lp_verify_structure_helper(plan->v.operand[1], LP_SET_OPERATION);
		break;
	case LP_TABLE:
		break;
	case LP_PROJECT:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_COLUMN_LIST);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_SELECT);
		break;
	case LP_SELECT:
		if ((NULL == plan->v.operand[0]) || (NULL == plan->v.operand[1]))
		{
			ret = FALSE;
			break;
		}
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_TABLE_JOIN);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_CRITERIA);
		break;
	case LP_TABLE_JOIN:
		if (NULL != plan->v.operand[0]) {
			ret &= lp_verify_structure_helper(plan->v.operand[0], LP_TABLE)
				| lp_verify_structure_helper(plan->v.operand[0], LP_INSERT)
				| lp_verify_structure_helper(plan->v.operand[0], LP_SET_OPERATION);
			ret &= lp_verify_structure_helper(plan->v.operand[1], LP_TABLE_JOIN);
		} else {
			ret &= (NULL == plan->v.operand[1]) ;
		}
		break;
	case LP_CRITERIA:
		if (NULL == plan->v.operand[0])
			break;
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEYS);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_SELECT_OPTIONS);
		break;
	case LP_KEYS:
		if (NULL != plan->v.operand[0]) {
			ret &= lp_verify_structure_helper(plan->v.operand[0], LP_KEY);
			ret &= lp_verify_structure_helper(plan->v.operand[1], LP_KEYS);
		} else {
			ret &= (NULL == plan->v.operand[1]);
		}
		break;
	case LP_KEY:
		break;
	case LP_KEY_FIX:
		ret = !((NULL == plan->v.operand[0]) || (NULL != plan->v.operand[1])
				|| (NULL == plan->v.operand[0]->v.key) || (NULL == plan->v.operand[0]->v.key->value));
		break;
	case LP_KEY_ADVANCE:
		ret = !((NULL == plan->v.operand[0]) || (NULL != plan->v.operand[1])
				|| (NULL == plan->v.operand[0]->v.key) || (NULL != plan->v.operand[0]->v.key->value)
				|| (NULL == plan->v.operand[0]->v.key->column));
		break;
	case LP_SELECT_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.operand[0], LP_WHERE);
		ret &= lp_verify_structure_helper(plan->v.operand[1], LP_KEYWORDS);
		break;
	case LP_KEYWORDS:
		// We allow NULL here
		break;
	case LP_SET_UNION:
	case LP_SET_INTERSECT:
	case LP_SET_EXCEPT:
	case LP_SET_UNION_ALL:
	case LP_SET_DNF:
	case LP_SET_INTERSECT_ALL:
	case LP_SET_EXCEPT_ALL:
		break;
	case LP_ADDITION:
	case LP_SUBTRACTION:
	case LP_DIVISION:
	case LP_MULTIPLICATION:
	case LP_MODULO:
	case LP_NEGATIVE:
		for (i = 0; i < 2; i++) {
			ret &= lp_verify_structure_helper(plan->v.operand[i], LP_ADDITION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_SUBTRACTION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_DIVISION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_MULTIPLICATION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_MODULO)
				| lp_verify_structure_helper(plan->v.operand[i], LP_NEGATIVE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_CASE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.operand[i], LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.operand[i], LP_VALUE)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. arithmetic operations etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.operand[i], LP_INSERT)
				| lp_verify_structure_helper(plan->v.operand[i], LP_SET_OPERATION);
		}
		break;
	case LP_CONCAT:
		for (i = 0; i < 2; i++) {
			ret &= lp_verify_structure_helper(plan->v.operand[i], LP_CASE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.operand[i], LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.operand[i], LP_VALUE)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. string concatenation operations etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.operand[i], LP_INSERT)
				| lp_verify_structure_helper(plan->v.operand[i], LP_SET_OPERATION);
		}
		break;
	case LP_BOOLEAN_OR:
	case LP_BOOLEAN_AND:
	case LP_BOOLEAN_IS:
	case LP_BOOLEAN_EQUALS:
	case LP_BOOLEAN_NOT_EQUALS:
	case LP_BOOLEAN_LESS_THAN:
	case LP_BOOLEAN_GREATER_THAN:
	case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
	case LP_BOOLEAN_REGEX_SENSITIVE:
	case LP_BOOLEAN_REGEX_INSENSITIVE:
	case LP_CASE_STATEMENT:
	case LP_CASE_BRANCH_STATEMENT:
	case LP_BOOLEAN_IN:
	case LP_BOOLEAN_NOT_IN:
	case LP_BOOLEAN_NOT:
	case LP_BOOLEAN_EXISTS:
	case LP_BOOLEAN_NOT_EXISTS:
	case LP_WHERE:
		for (i = 0; i < 2; i++) {
			boolean_t	is_where;

			if ((LP_BOOLEAN_NOT == expected) || (LP_BOOLEAN_EXISTS == expected)
					|| (LP_BOOLEAN_NOT_EXISTS == expected)) {
				ret &= (NULL == plan->v.operand[1]);
				break;
			}
			is_where = ((1 == i) && (LP_WHERE == expected));
			ret &= lp_verify_structure_helper(plan->v.operand[i], LP_ADDITION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_SUBTRACTION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_DIVISION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_MULTIPLICATION)
				| lp_verify_structure_helper(plan->v.operand[i], LP_MODULO)
				| lp_verify_structure_helper(plan->v.operand[i], LP_NEGATIVE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_CASE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_CONCAT)
				| lp_verify_structure_helper(plan->v.operand[i], LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.operand[i], LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.operand[i], LP_VALUE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_OR)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_AND)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_IS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_EQUALS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_NOT_EQUALS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_LESS_THAN)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_GREATER_THAN)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_LESS_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_GREATER_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_REGEX_SENSITIVE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_REGEX_INSENSITIVE)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_IN)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_NOT_IN)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_NOT)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_EXISTS)
				| lp_verify_structure_helper(plan->v.operand[i], LP_BOOLEAN_NOT_EXISTS)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. arithmetic operations etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.operand[i], LP_INSERT)
				| lp_verify_structure_helper(plan->v.operand[i], LP_SET_OPERATION)
				// LP_COLUMN_LIST_ALIAS is possible as operand[1] only for LP_WHERE. Check that.
				| (is_where && lp_verify_structure_helper(plan->v.operand[i], LP_COLUMN_LIST_ALIAS));
		}
		break;
	case LP_COLUMN_LIST:
		/* to avoid a large recursion stack walk the column list iteratively */
		while (NULL != plan) {
			assert(LP_COLUMN_LIST == plan->type);
		        ret &= lp_verify_structure_helper(plan->v.operand[0], LP_WHERE)
				| lp_verify_structure_helper(plan->v.operand[0], LP_ADDITION)
				| lp_verify_structure_helper(plan->v.operand[0], LP_SUBTRACTION)
				| lp_verify_structure_helper(plan->v.operand[0], LP_MULTIPLICATION)
				| lp_verify_structure_helper(plan->v.operand[0], LP_DIVISION)
				| lp_verify_structure_helper(plan->v.operand[0], LP_MODULO)
				| lp_verify_structure_helper(plan->v.operand[0], LP_NEGATIVE)
				| lp_verify_structure_helper(plan->v.operand[0], LP_CONCAT)
				| lp_verify_structure_helper(plan->v.operand[0], LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.operand[0], LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.operand[0], LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.operand[0], LP_VALUE)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. select column list etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.operand[0], LP_INSERT)
				| lp_verify_structure_helper(plan->v.operand[0], LP_SET_OPERATION);
			assert(ret);
			plan = plan->v.operand[1];
		}
		break;
	case LP_COLUMN_ALIAS:
	case LP_DERIVED_COLUMN:
	case LP_PIECE_NUMBER:
	case LP_VALUE:
	case LP_COLUMN_LIST_ALIAS:
		// This has no children to check
		break;
	case LP_FUNCTION_CALL:
	        ret &= lp_verify_structure_helper(plan->v.operand[0], LP_VALUE);
	        ret &= lp_verify_structure_helper(plan->v.operand[1], LP_COLUMN_LIST);
		break;
	case LP_CASE:
	        ret &= lp_verify_structure_helper(plan->v.operand[0], LP_CASE_STATEMENT);
	        ret &= lp_verify_structure_helper(plan->v.operand[1], LP_CASE_BRANCH);
		break;
	case LP_CASE_BRANCH:
	        ret &= lp_verify_structure_helper(plan->v.operand[0], LP_CASE_BRANCH_STATEMENT);
	        ret &= lp_verify_structure_helper(plan->v.operand[1], LP_CASE_BRANCH);
		break;
	default:
		// This should never happen
		assert(FALSE);
	}
	assert(ret);
	return ret;
}
