/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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
#include "physical_plan.h"
#include "lp_verify_structure.h"

static int	 lp_verify_structure_helper(LogicalPlan *plan, PhysicalPlanOptions *options, LPActionType expected);
static boolean_t lp_verify_value(LogicalPlan *plan, PhysicalPlanOptions *options);

/* Verifies the given LP has a good structure; return TRUE if it is all good and FALSE otherwise */
int lp_verify_structure(LogicalPlan *plan, PhysicalPlanOptions *options) {
	switch (plan->type) {
	case LP_SELECT_QUERY:
	case LP_SET_OPERATION:
	case LP_TABLE_VALUE:
	case LP_INSERT_INTO:
	case LP_DELETE_FROM:
	case LP_UPDATE:
		break;
	default:
		assert(FALSE);
		return FALSE;
	}
	return lp_verify_structure_helper(plan, options, plan->type);
}

int lp_verify_structure_helper(LogicalPlan *plan, PhysicalPlanOptions *options, LPActionType expected) {
	int i, ret = TRUE;

	// Cases where NULL is not allowed is enforced in the switch below
	if (NULL == plan)
		return TRUE;
	if (plan->type != expected)
		return FALSE;
	switch (expected) {
	case LP_SELECT_QUERY:
		/* If "options->aggregate" is non-NULL, check if the aggregate corresponds to this LP_SELECT_QUERY.
		 * If so, descend down this plan. If not, we will come to this LP_SELECT_QUERY as part of a
		 * different `lp_verify_structure` call. Wait until then. This is because we do not want to
		 * fill in aggregate function information (first_aggregate/next_aggregate linked list and
		 * aggregate_cnt) across different logical plans.
		 */
		if ((NULL == options) || (options->aggregate == &plan->extra_detail.lp_select_query.first_aggregate)) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_PROJECT);
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_OUTPUT);
		}
		break;
	case LP_INSERT_INTO:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_INSERT_INTO_OPTIONS);
		break;
	case LP_INSERT_INTO_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_INSERT_INTO_MORE_OPTIONS);
		break;
	case LP_INSERT_INTO_MORE_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SELECT_QUERY)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_OPERATION)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE_VALUE);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_CHECK_CONSTRAINT);
		break;
	case LP_DELETE_FROM:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_PROJECT);
		ret &= (NULL == plan->v.lp_default.operand[1]);
		break;
	case LP_UPDATE:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_PROJECT);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_COLUMN_LIST);
		break;
	case LP_OUTPUT:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_KEY);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_ORDER_BY);
		break;
	case LP_SET_OPERATION:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_OPTION);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_PLANS);
		break;
	case LP_SET_OPTION:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_UNION)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_UNION_ALL)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_EXCEPT)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_EXCEPT_ALL)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_INTERSECT)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_INTERSECT_ALL)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_DNF);
		assert((LP_SET_DNF != plan->v.lp_default.operand[0]->type) || (NULL == plan->v.lp_default.operand[1]));
		assert((LP_SET_DNF == plan->v.lp_default.operand[0]->type) || (NULL != plan->v.lp_default.operand[1]));
		if (NULL != plan->v.lp_default.operand[1]) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_OUTPUT);
		}
		break;
	case LP_PLANS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SELECT_QUERY)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE_VALUE)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_OPERATION);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_SELECT_QUERY)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_TABLE_VALUE)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_SET_OPERATION);
		break;
	case LP_TABLE:
		/* The below logic is very similar to the logic in the LP_FUNCTION_CALL case when "options" is non-NULL.
		 * See comments there for more details.
		 */
		if ((NULL != options) && (NULL == plan->extra_detail.lp_table.next_table)) {
			LogicalPlan *prev_table;

			prev_table = *options->table;
			assert(prev_table != plan); /* Otherwise we would end up in an infinite loop later during
						     * template file generation.
						     */
			if (NULL == prev_table) {
				/* See comment where LP_LIST_END macro is defined for details on this special value */
				prev_table = LP_LIST_END;
			}
			plan->extra_detail.lp_table.next_table = prev_table;
			*options->table = plan;
		}
		break;
	case LP_TABLE_VALUE:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE_DATA);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_OUTPUT);
		break;
	case LP_TABLE_DATA:
	case LP_ROW_VALUE:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_ROW_VALUE);
		break;
	case LP_PROJECT:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_SELECT);
		break;
	case LP_SELECT:
		if ((NULL == plan->v.lp_default.operand[0]) || (NULL == plan->v.lp_default.operand[1])) {
			ret = FALSE;
			break;
		}
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE_JOIN);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_CRITERIA);
		break;
	case LP_TABLE_JOIN:
		if (NULL != plan->v.lp_default.operand[0]) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE_VALUE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SELECT_QUERY)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_OPERATION);
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_TABLE_JOIN);
			ret &= lp_verify_structure_helper(plan->extra_detail.lp_table_join.join_on_condition, options, LP_WHERE);
		} else {
			ret &= (NULL == plan->v.lp_default.operand[1]);
		}
		break;
	case LP_CRITERIA:
		if (NULL == plan->v.lp_default.operand[0])
			break;
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_KEYS);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_SELECT_OPTIONS);
		break;
	case LP_KEYS:
		if (NULL != plan->v.lp_default.operand[0]) {
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_KEY);
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_KEYS);
		} else {
			ret &= (NULL == plan->v.lp_default.operand[1]);
		}
		break;
	case LP_KEY: {
		SqlKey *key;

		key = plan->v.lp_key.key;
		switch (key->type) {
		case LP_KEY_FIX:
			ret = !((NULL == plan->v.lp_default.operand[0]) || (NULL != plan->v.lp_default.operand[1]) || (NULL == key)
				|| (NULL == key->fixed_to_value));
			break;
		case LP_KEY_ADVANCE:
			ret = !((NULL == plan->v.lp_default.operand[0]) || (NULL != plan->v.lp_default.operand[1]) || (NULL == key)
				|| (NULL != key->fixed_to_value));
			break;
		case LP_INVALID_ACTION:
			/* This has to be the output key in an xref plan (generated in "lp_generate_xref_plan.c") */
			ret = key->is_cross_reference_key;
			break;
		default:
			assert(FALSE);
			break;
		}
		break;
	}
	case LP_SELECT_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_WHERE);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_SELECT_MORE_OPTIONS);
		break;
	case LP_SELECT_MORE_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_AGGREGATE_OPTIONS);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_KEYWORDS);
		break;
	case LP_AGGREGATE_OPTIONS:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_GROUP_BY);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_HAVING);
		break;
	case LP_KEYWORDS:
		// We allow NULL here
		break;
	case LP_GROUP_BY:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		ret &= (NULL == plan->v.lp_default.operand[1]);
		break;
	case LP_HAVING:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_WHERE);
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
	case LP_CONCAT:
		for (i = 0; i < 2; i++) {
			if ((1 == i) && ((LP_NEGATIVE == expected) || (LP_FORCE_NUM == expected) || (LP_COERCE_TYPE == expected))) {
				/* Unary operation. So second operand should be NULL. */
				ret &= (NULL == plan->v.lp_default.operand[1]);
				break;
			}
			ret &= lp_verify_value(plan->v.lp_default.operand[i], options);
		}
		break;
	case LP_BOOLEAN_OR:
	case LP_BOOLEAN_AND:
	case LP_BOOLEAN_IS:
	case LP_BOOLEAN_IS_NOT:
	case LP_BOOLEAN_EQUALS:
	case LP_BOOLEAN_NOT_EQUALS:
	case LP_BOOLEAN_LESS_THAN:
	case LP_BOOLEAN_GREATER_THAN:
	case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
	case LP_BOOLEAN_REGEX_SENSITIVE:
	case LP_BOOLEAN_REGEX_INSENSITIVE:
	case LP_BOOLEAN_REGEX_SENSITIVE_LIKE:
	case LP_BOOLEAN_REGEX_SENSITIVE_SIMILARTO:
	case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE:
	case LP_BOOLEAN_REGEX_INSENSITIVE_SIMILARTO:
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
			boolean_t is_where, is_bool_in;

			if ((1 == i)
			    && ((LP_BOOLEAN_NOT == expected) || (LP_BOOLEAN_EXISTS == expected)
				|| (LP_BOOLEAN_NOT_EXISTS == expected))) {
				ret &= (NULL == plan->v.lp_default.operand[1]);
				break;
			}
			is_where = ((1 == i) && (LP_WHERE == expected));
			is_bool_in = ((1 == i) && ((LP_BOOLEAN_IN == expected) || (LP_BOOLEAN_NOT_IN == expected)));
			ret &= lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_ADDITION)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_SUBTRACTION)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_DIVISION)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_MULTIPLICATION)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_MODULO)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_NEGATIVE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_FORCE_NUM)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_CASE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_CONCAT)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_COLUMN_ALIAS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_INSERT_INTO_COL)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_FUNCTION_CALL)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_ARRAY)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_COALESCE_CALL)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_GREATEST)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_LEAST)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_NULL_IF)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_AGGREGATE_FUNCTION_COUNT_ASTERISK)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_AGGREGATE_FUNCTION_COUNT)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_AGGREGATE_FUNCTION_AVG)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_AGGREGATE_FUNCTION_MIN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_AGGREGATE_FUNCTION_MAX)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_AGGREGATE_FUNCTION_SUM)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_AGGREGATE_FUNCTION_COUNT_DISTINCT)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_AGGREGATE_FUNCTION_AVG_DISTINCT)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_AGGREGATE_FUNCTION_SUM_DISTINCT)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_DERIVED_COLUMN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_VALUE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_COERCE_TYPE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_OR)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_AND)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_IS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_IS_NOT)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_NOT_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_LESS_THAN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_GREATER_THAN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_LESS_THAN_OR_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_GREATER_THAN_OR_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_REGEX_SENSITIVE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_REGEX_INSENSITIVE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_REGEX_SENSITIVE_LIKE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_REGEX_SENSITIVE_SIMILARTO)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_REGEX_INSENSITIVE_LIKE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_REGEX_INSENSITIVE_SIMILARTO)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_IN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_NOT_IN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_NOT)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ANY_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ANY_NOT_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ANY_LESS_THAN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ANY_GREATER_THAN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_ANY_LESS_THAN_OR_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_ANY_GREATER_THAN_OR_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ALL_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ALL_NOT_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ALL_LESS_THAN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_ALL_GREATER_THAN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_ALL_LESS_THAN_OR_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options,
							    LP_BOOLEAN_ALL_GREATER_THAN_OR_EQUALS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_EXISTS)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_BOOLEAN_NOT_EXISTS)
			       /* LP_BOOLEAN_IN and LP_BOOLEAN_NOT_IN can have a LP_COLUMN_LIST as operand 1 */
			       | (is_bool_in && lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_COLUMN_LIST))
			       /* LP_SELECT_QUERY/LP_TABLE_VALUE/LP_SET_OPERATIONs usually show up as operand[1] only for the IN
				* boolean expression. But they can show up wherever a scalar is expected (e.g. arithmetic
				* operations etc.) and hence have to be allowed in a lot more cases.
				*/
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_SELECT_QUERY)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_TABLE_VALUE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_SET_OPERATION)
			       // LP_COLUMN_LIST_ALIAS is possible as operand[1] only for LP_WHERE. Check that.
			       | (is_where
				  && lp_verify_structure_helper(plan->v.lp_default.operand[i], options, LP_COLUMN_LIST_ALIAS));
		}
		break;
	case LP_UPD_COL_VALUE:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN);
		ret &= lp_verify_value(plan->v.lp_default.operand[1], options);
		break;
	case LP_CHECK_CONSTRAINT:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_WHERE);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_CHECK_CONSTRAINT);
		break;
	case LP_COLUMN_LIST:
		/* To avoid a large recursion stack in case of thousands of columns, walk the column list iteratively */
		while (NULL != plan) {
			assert(LP_COLUMN_LIST == plan->type);
			/* LP_COLUMN and LP_UPD_COL_VALUE are only possible inside a LP_COLUMN_LIST.
			 * Hence the additional check for those below.
			 */
			ret &= lp_verify_value(plan->v.lp_default.operand[0], options)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_UPD_COL_VALUE)
			       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_CHECK_CONSTRAINT);
			assert(ret);
			plan = plan->v.lp_default.operand[1];
		}
		break;
	case LP_VALUE:
	case LP_COLUMN_ALIAS:
	case LP_DERIVED_COLUMN:
	case LP_INSERT_INTO_COL:
	case LP_PIECE_NUMBER:
	case LP_COLUMN_LIST_ALIAS:
	case LP_COLUMN:
		// This has no children to check
		break;
	case LP_FUNCTION_CALL:
		/* If "options" is non-NULL, it is possible "plan->extra_detail.lp_function_call.next_function" is non-NULL
		 * due to DNF expansion of plans. For example the below query
		 *	"select * from names where (ABS(id) = 1) and (firstname = 'abcd' OR lastname = 'efgh');"
		 * would get DNF expanded to
		 *	"select * from names where (ABS(id) = 1) and (firstname = 'abcd')"
		 *				OR
		 *	"select * from names where (ABS(id) = 1) and (lastname = 'efgh')"
		 * where the "ABS(id)" plan is the same in both DNF plans and so would have been its "next_function" pointer
		 * set up already when "ABS(id)" is encountered for the second DNF plan. In this case, we can skip the
		 * "next_function" linked list maintenance since it is already part of the linked list for the entire query.
		 * Hence the right side of the "&&" in the below "if" check.
		 */
		if ((NULL != options) && (NULL == plan->extra_detail.lp_function_call.next_function)) {
			LogicalPlan *prev_function;

			prev_function = *options->function;
			assert(prev_function != plan); /* Otherwise we would end up in an infinite loop later during
							* template file generation.
							*/
			if (NULL == prev_function) {
				/* See comment where LP_LIST_END macro is defined for details on this special value */
				prev_function = LP_LIST_END;
			}
			plan->extra_detail.lp_function_call.next_function = prev_function;
			*options->function = plan;
		}
		assert(LP_VALUE == plan->v.lp_default.operand[0]->type);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_VALUE);
		assert(LP_COLUMN_LIST == plan->v.lp_default.operand[1]->type);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_COLUMN_LIST);
		break;
	case LP_ARRAY:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SELECT_QUERY)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_TABLE_VALUE)
		       | lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_SET_OPERATION);
		break;
	case LP_COALESCE_CALL:
		assert(LP_COLUMN_LIST == plan->v.lp_default.operand[0]->type);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		break;
	case LP_GREATEST:
		assert(LP_COLUMN_LIST == plan->v.lp_default.operand[0]->type);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		break;
	case LP_LEAST:
		assert(LP_COLUMN_LIST == plan->v.lp_default.operand[0]->type);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		break;
	case LP_NULL_IF:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, plan->v.lp_default.operand[0]->type);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, plan->v.lp_default.operand[1]->type);
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
				ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
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
	case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK:
	case LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK:
		if (NULL != options) {
			int	     prev_aggregate_cnt;
			LogicalPlan *prev_aggregate;

			prev_aggregate = *options->aggregate;
			assert(prev_aggregate != plan); /* otherwise we would end up in an infinite loop later during
							 * template file generation (see YDBOcto#456 for example).
							 */
			assert(NULL == plan->extra_detail.lp_aggregate_function.next_aggregate);
			plan->extra_detail.lp_aggregate_function.next_aggregate = prev_aggregate;
			prev_aggregate_cnt
			    = ((NULL == prev_aggregate) ? 0 : prev_aggregate->extra_detail.lp_aggregate_function.aggregate_cnt);
			plan->extra_detail.lp_aggregate_function.aggregate_cnt = prev_aggregate_cnt + 1;
			*options->aggregate = plan;
		}
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_COLUMN_LIST);
		/* In case count(DISTINCT table.*) usage we can expect operand[1] to have another LP_COLUMN_LIST */
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_COLUMN_LIST);
		break;
	case LP_CASE:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_CASE_STATEMENT);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_CASE_BRANCH);
		break;
	case LP_CASE_BRANCH:
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[0], options, LP_CASE_BRANCH_STATEMENT);
		ret &= lp_verify_structure_helper(plan->v.lp_default.operand[1], options, LP_CASE_BRANCH);
		break;
	default:
		// This should never happen
		assert(FALSE);
		break;
	}
	assert(ret);
	return ret;
}

boolean_t lp_verify_value(LogicalPlan *plan, PhysicalPlanOptions *options) {
	return lp_verify_structure_helper(plan, options, LP_WHERE) | lp_verify_structure_helper(plan, options, LP_ADDITION)
	       | lp_verify_structure_helper(plan, options, LP_SUBTRACTION)
	       | lp_verify_structure_helper(plan, options, LP_MULTIPLICATION)
	       | lp_verify_structure_helper(plan, options, LP_DIVISION) | lp_verify_structure_helper(plan, options, LP_MODULO)
	       | lp_verify_structure_helper(plan, options, LP_NEGATIVE) | lp_verify_structure_helper(plan, options, LP_FORCE_NUM)
	       | lp_verify_structure_helper(plan, options, LP_CONCAT) | lp_verify_structure_helper(plan, options, LP_COLUMN_ALIAS)
	       | lp_verify_structure_helper(plan, options, LP_INSERT_INTO_COL) | lp_verify_structure_helper(plan, options, LP_CASE)
	       | lp_verify_structure_helper(plan, options, LP_FUNCTION_CALL) | lp_verify_structure_helper(plan, options, LP_ARRAY)
	       | lp_verify_structure_helper(plan, options, LP_COALESCE_CALL)
	       | lp_verify_structure_helper(plan, options, LP_GREATEST) | lp_verify_structure_helper(plan, options, LP_LEAST)
	       | lp_verify_structure_helper(plan, options, LP_NULL_IF)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_COUNT_ASTERISK)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_COUNT)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_AVG)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_MIN)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_MAX)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_SUM)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_COUNT_DISTINCT)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_AVG_DISTINCT)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_SUM_DISTINCT)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK)
	       | lp_verify_structure_helper(plan, options, LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK)
	       | lp_verify_structure_helper(plan, options, LP_DERIVED_COLUMN) | lp_verify_structure_helper(plan, options, LP_VALUE)
	       | lp_verify_structure_helper(plan, options, LP_COERCE_TYPE)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_IN)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_NOT_IN)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_NOT)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_OR)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_AND)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_IS)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_IS_NOT)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_EQUALS)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_NOT_EQUALS)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_LESS_THAN)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_GREATER_THAN)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_LESS_THAN_OR_EQUALS)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_GREATER_THAN_OR_EQUALS)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_EXISTS)
	       | lp_verify_structure_helper(plan, options, LP_BOOLEAN_NOT_EXISTS)
	       /* LP_SELECT_QUERY/LP_TABLE_VALUE/LP_SET_OPERATIONs usually show up as operand[1] only for the IN boolean
		* expression. But they can show up wherever a scalar is expected (e.g. select column list etc.)
		* and hence have to be allowed in a lot more cases.
		*/
	       | lp_verify_structure_helper(plan, options, LP_SELECT_QUERY)
	       | lp_verify_structure_helper(plan, options, LP_TABLE_VALUE)
	       | lp_verify_structure_helper(plan, options, LP_SET_OPERATION);
}
