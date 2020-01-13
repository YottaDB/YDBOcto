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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

int lp_verify_structure_helper(LogicalPlan *plan, LogicalPlan **aggregate, LPActionType expected);

/* Verifies the given LP has a good structure; return TRUE if it is all good and FALSE otherwise */
int lp_verify_structure(LogicalPlan *plan, LogicalPlan **aggregate) {
	if ((LP_INSERT != plan->type) && (LP_SET_OPERATION != plan->type))
	{
		assert(FALSE);
		return FALSE;
	}
	return lp_verify_structure_helper(plan, aggregate, plan->type);
}

int lp_verify_structure_helper(LogicalPlan *plan, LogicalPlan **aggregate, LPActionType expected) {
	int		i, ret = TRUE;

	// Cases where NULL is not allowed is enforced in the switch below
	if (NULL == plan)
		return TRUE;
	if (plan->type != expected)
		return FALSE;
	switch (expected) {
	case LP_INSERT:
		/* If the "aggregate" is non-NULL, check if the aggregate corresponds to this LP_INSERT.
		 * If so, descend down this plan. If not, we will come to this LP_INSERT as part of a
		 * different `lp_verify_structure` call. Wait until then. This is because we do not want to
		 * fill in aggregate function information (first_aggregate/next_aggregate linked list and
		 * aggregate_cnt) across different logical plans.
		 */
		if ((NULL == aggregate) || (aggregate == &plan->extra_detail.lp_insert.first_aggregate)) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_PROJECT);
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_OUTPUT);
		}
		break;
	case LP_OUTPUT:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_KEY);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_ORDER_BY);
		break;
	case LP_SET_OPERATION:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_OPTION);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_PLANS);
		break;
	case LP_SET_OPTION:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_UNION)
			| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_UNION_ALL)
			| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_EXCEPT)
			| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_EXCEPT_ALL)
			| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_INTERSECT)
			| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_INTERSECT_ALL)
			| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_DNF);
		assert((LP_SET_DNF != plan->v.lp_default.operand[0]->type) || (NULL == plan->v.lp_default.operand[1]));
		assert((LP_SET_DNF == plan->v.lp_default.operand[0]->type) || (NULL != plan->v.lp_default.operand[1]));
		if (NULL != plan->v.lp_default.operand[1]) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_OUTPUT);
		}
		break;
	case LP_PLANS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_INSERT)
			| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_OPERATION);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_INSERT)
			| lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_SET_OPERATION);
		break;
	case LP_TABLE:
		break;
	case LP_PROJECT:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_COLUMN_LIST);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_SELECT);
		break;
	case LP_SELECT:
		if ((NULL == plan->v.lp_default.operand[0]) || (NULL == plan->v.lp_default.operand[1]))
		{
			ret = FALSE;
			break;
		}
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_TABLE_JOIN);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_CRITERIA);
		break;
	case LP_TABLE_JOIN:
		if (NULL != plan->v.lp_default.operand[0]) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_TABLE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_INSERT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_OPERATION);
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_TABLE_JOIN);
			ret &= lp_verify_structure_helper(plan->extra_detail.lp_table_join.join_on_condition, aggregate, LP_WHERE);
		} else {
			ret &= (NULL == plan->v.lp_default.operand[1]) ;
		}
		break;
	case LP_CRITERIA:
		if (NULL == plan->v.lp_default.operand[0])
			break;
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_KEYS);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_SELECT_OPTIONS);
		break;
	case LP_KEYS:
		if (NULL != plan->v.lp_default.operand[0]) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_KEY);
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_KEYS);
		} else {
			ret &= (NULL == plan->v.lp_default.operand[1]);
		}
		break;
	case LP_KEY:
		break;
	case LP_KEY_FIX:
		ret = !((NULL == plan->v.lp_default.operand[0]) || (NULL != plan->v.lp_default.operand[1])
				|| (NULL == plan->v.lp_default.operand[0]->v.lp_key.key)
				|| (NULL == plan->v.lp_default.operand[0]->v.lp_key.key->fixed_to_value));
		break;
	case LP_KEY_ADVANCE:
		ret = !((NULL == plan->v.lp_default.operand[0]) || (NULL != plan->v.lp_default.operand[1])
				|| (NULL == plan->v.lp_default.operand[0]->v.lp_key.key)
				|| (NULL != plan->v.lp_default.operand[0]->v.lp_key.key->fixed_to_value)
				|| (NULL == plan->v.lp_default.operand[0]->v.lp_key.key->column));
		break;
	case LP_SELECT_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_WHERE);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_SELECT_MORE_OPTIONS);
		break;
	case LP_SELECT_MORE_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_AGGREGATE_OPTIONS);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_KEYWORDS);
		break;
	case LP_AGGREGATE_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_GROUP_BY);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_HAVING);
		break;
	case LP_KEYWORDS:
		// We allow NULL here
		break;
	case LP_GROUP_BY:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_COLUMN_LIST);
		ret &= (NULL == plan->v.lp_default.operand[1]);
		break;
	case LP_HAVING:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_WHERE);
		ret &= (NULL == plan->v.lp_default.operand[1]);
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
	case LP_FORCE_NUM:
	case LP_COERCE_TYPE:
		for (i = 0; i < 2; i++) {
			if ((1 == i) && ((LP_NEGATIVE == expected) || (LP_FORCE_NUM == expected) || (LP_COERCE_TYPE == expected))) {
				/* Unary operation. So second operand should be NULL. */
				ret &= (NULL == plan->v.lp_default.operand[1]);
				break;
			}
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_ADDITION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_SUBTRACTION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_DIVISION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_MULTIPLICATION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_MODULO)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_NEGATIVE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_FORCE_NUM)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_CASE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_ASTERISK)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_COUNT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_AVG)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_MIN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_MAX)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_SUM)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_AVG_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_SUM_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_VALUE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_COERCE_TYPE)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. arithmetic operations etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_INSERT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_SET_OPERATION);
		}
		break;
	case LP_CONCAT:
		for (i = 0; i < 2; i++) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_CASE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_ASTERISK)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_COUNT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_AVG)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_MIN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_MAX)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_SUM)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_AVG_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_SUM_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_VALUE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_COERCE_TYPE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_CONCAT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_NEGATIVE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_FORCE_NUM)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. string concatenation operations etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_INSERT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_SET_OPERATION);
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
	case LP_BOOLEAN_ANY_EQUALS:
	case LP_BOOLEAN_ANY_NOT_EQUALS:
	case LP_BOOLEAN_ANY_LESS_THAN:
	case LP_BOOLEAN_ANY_GREATER_THAN:
	case LP_BOOLEAN_ANY_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_ANY_GREATER_THAN_OR_EQUALS:
	case LP_BOOLEAN_ALL_EQUALS:
	case LP_BOOLEAN_ALL_NOT_EQUALS:
	case LP_BOOLEAN_ALL_LESS_THAN:
	case LP_BOOLEAN_ALL_GREATER_THAN:
	case LP_BOOLEAN_ALL_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_ALL_GREATER_THAN_OR_EQUALS:
	case LP_BOOLEAN_EXISTS:
	case LP_BOOLEAN_NOT_EXISTS:
	case LP_WHERE:
		for (i = 0; i < 2; i++) {
			boolean_t	is_where;

			if ((1 == i) && ((LP_BOOLEAN_NOT == expected) || (LP_BOOLEAN_EXISTS == expected)
					|| (LP_BOOLEAN_NOT_EXISTS == expected))) {
				ret &= (NULL == plan->v.lp_default.operand[1]);
				break;
			}
			is_where = ((1 == i) && (LP_WHERE == expected));
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_ADDITION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_SUBTRACTION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_DIVISION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_MULTIPLICATION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_MODULO)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_NEGATIVE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_FORCE_NUM)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_CASE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_CONCAT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_ASTERISK)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_COUNT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_AVG)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_MIN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_MAX)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_AGGREGATE_FUNCTION_SUM)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_AVG_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_AGGREGATE_FUNCTION_SUM_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_VALUE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_COERCE_TYPE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_OR)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_AND)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_IS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_NOT_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_LESS_THAN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_GREATER_THAN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_BOOLEAN_LESS_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_BOOLEAN_GREATER_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_REGEX_SENSITIVE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_REGEX_INSENSITIVE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_IN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_NOT_IN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_NOT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ANY_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ANY_NOT_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ANY_LESS_THAN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ANY_GREATER_THAN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_BOOLEAN_ANY_LESS_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_BOOLEAN_ANY_GREATER_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ALL_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ALL_NOT_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ALL_LESS_THAN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_ALL_GREATER_THAN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_BOOLEAN_ALL_LESS_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_BOOLEAN_ALL_GREATER_THAN_OR_EQUALS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_EXISTS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_BOOLEAN_NOT_EXISTS)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. arithmetic operations etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_INSERT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate, LP_SET_OPERATION)
				// LP_COLUMN_LIST_ALIAS is possible as operand[1] only for LP_WHERE. Check that.
				| (is_where && lp_verify_structure_helper(plan->v.lp_default.operand[i], aggregate,
									LP_COLUMN_LIST_ALIAS));
		}
		break;
	case LP_COLUMN_LIST:
		/* To avoid a large recursion stack in case of thousands of columns, walk the column list iteratively */
		while (NULL != plan) {
			assert(LP_COLUMN_LIST == plan->type);
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_WHERE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_ADDITION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SUBTRACTION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_MULTIPLICATION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_DIVISION)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_MODULO)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_NEGATIVE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_FORCE_NUM)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_CONCAT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_COLUMN_ALIAS)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_FUNCTION_CALL)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_ASTERISK)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_AGGREGATE_FUNCTION_AVG)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_AGGREGATE_FUNCTION_MIN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_AGGREGATE_FUNCTION_MAX)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_AGGREGATE_FUNCTION_SUM)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate,
									LP_AGGREGATE_FUNCTION_COUNT_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate,
									LP_AGGREGATE_FUNCTION_AVG_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate,
									LP_AGGREGATE_FUNCTION_SUM_DISTINCT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_DERIVED_COLUMN)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_VALUE)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_COERCE_TYPE)
				// LP_INSERT/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean expression.
				// But they can show up wherever a scalar is expected (e.g. select column list etc.)
				// and hence have to be allowed in a lot more cases.
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_INSERT)
				| lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_SET_OPERATION);
			assert(ret);
			plan = plan->v.lp_default.operand[1];
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
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_VALUE);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_COLUMN_LIST);
		break;
	case LP_ORDER_BY:
		/* to avoid a large recursion stack walk the column list iteratively */
		while (NULL != plan) {
			assert(LP_ORDER_BY == plan->type);
			/* In the ORDER BY COLUMN NUM case, the ORDER BY plan (LP_ORDER_BY -> LP_COLUMN_LIST -> LP_WHERE)
			 * points to the same plan as the SELECT column list (LP_PROJECT -> LP_COLUMN_LIST -> ... -> LP_WHERE)
			 * and so do not descend down the ORDER BY plan to avoid duplicate descents as well as avoid
			 * confusion (e.g. cycles in linked list) in case we need to construct the
			 * first_aggregate/next_aggregate linked list.
			 */
			if (!plan->extra_detail.lp_order_by.order_by_column_num) {
				ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_COLUMN_LIST);
				assert(ret);
			}
			plan = plan->v.lp_default.operand[1];
		}
		break;
	case LP_AGGREGATE_FUNCTION_COUNT_ASTERISK:
	case LP_AGGREGATE_FUNCTION_COUNT:
	case LP_AGGREGATE_FUNCTION_AVG:
	case LP_AGGREGATE_FUNCTION_MIN:
	case LP_AGGREGATE_FUNCTION_MAX:
	case LP_AGGREGATE_FUNCTION_SUM:
	case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT:
	case LP_AGGREGATE_FUNCTION_AVG_DISTINCT:
	case LP_AGGREGATE_FUNCTION_SUM_DISTINCT:
		if (NULL != aggregate) {
			int		prev_aggregate_cnt;
			LogicalPlan	*prev_aggregate;

			prev_aggregate = *aggregate;
			plan->extra_detail.lp_aggregate_function.next_aggregate = prev_aggregate;
			prev_aggregate_cnt = ((NULL == prev_aggregate)
						? 0
						: prev_aggregate->extra_detail.lp_aggregate_function.aggregate_cnt);
			plan->extra_detail.lp_aggregate_function.aggregate_cnt = prev_aggregate_cnt + 1;
			*aggregate = plan;
		}
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_COLUMN_LIST);
		ret &= (NULL == plan->v.lp_default.operand[1]);
		break;
	case LP_CASE:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_CASE_STATEMENT);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_CASE_BRANCH);
		break;
	case LP_CASE_BRANCH:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], aggregate, LP_CASE_BRANCH_STATEMENT);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], aggregate, LP_CASE_BRANCH);
		break;
	default:
		// This should never happen
		assert(FALSE);
		break;
	}
	assert(ret);
	return ret;
}
