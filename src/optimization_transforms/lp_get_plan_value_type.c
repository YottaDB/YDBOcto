/****************************************************************
 *								*
 * Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	*
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

/*
 */
SqlValueType lp_get_plan_value_type(LogicalPlan *plan) {
	boolean_t	loop_done;
	LogicalPlan    *cur_plan, *ret_type_plan;
	SqlColumnAlias *column_alias;
	SqlValueType	return_type = UNKNOWN_SqlValueType;

	/* We assume all values in this expression have the same type, which should be true due to the matching of types
	 * further up the stack (in `populate_data_type`). Traverse down the left side of the logical plan tree until we get
	 * to a plan node which has only a left child (right hand child is NULL) OR stop traversing if we end up with
	 * determine the type of that node
	 */
	cur_plan = plan;
	for (loop_done = FALSE; !loop_done;) {
		switch (cur_plan->type) {
		case LP_DATE_TIME_SUBTRACTION:
		case LP_DATE_TIME_ADDITION:
			return_type = cur_plan->extra_detail.lp_date_time_operation.return_type;
			loop_done = TRUE;
			break;
		case LP_CONCAT:
			return_type = STRING_LITERAL; // This is always true
			loop_done = TRUE;
			break;
		case LP_VALUE:
			assert(BOOLEAN_OR_STRING_LITERAL != cur_plan->v.lp_value.value->type);
			return_type = cur_plan->v.lp_value.value->type;
			loop_done = TRUE;
			break;
		case LP_COERCE_TYPE:
			return_type
			    = get_sqlvaluetype_from_sqldatatype(cur_plan->extra_detail.lp_coerce_type.coerce_type.data_type, FALSE);
			loop_done = TRUE;
			break;
		case LP_COLUMN_ALIAS:
		case LP_DERIVED_COLUMN:
		case LP_INSERT_INTO_COL:
		case LP_UPDATE_COL:
			switch (cur_plan->type) {
			case LP_COLUMN_ALIAS:
				column_alias = cur_plan->v.lp_column_alias.column_alias;
				break;
			case LP_DERIVED_COLUMN:
				column_alias = cur_plan->extra_detail.lp_derived_column.subquery_column_alias;
				break;
			case LP_INSERT_INTO_COL:
				column_alias = cur_plan->v.lp_insert_into_col.column_alias;
				break;
			default:
				assert(LP_UPDATE_COL == cur_plan->type);
				column_alias = cur_plan->v.lp_update_col.column_alias;
				break;
			}
			switch (column_alias->column->type) {
			case value_STATEMENT:
				if (TABLE_ASTERISK == column_alias->column->v.value->type) {
					// Treat table.* as a string.
					return_type = STRING_LITERAL;
				} else {
					return_type = column_alias->column->v.value->type;
				}
				break;
			case column_list_alias_STATEMENT:
				assert(BOOLEAN_OR_STRING_LITERAL != column_alias->column->v.column_list_alias->type);
				return_type = column_alias->column->v.column_list_alias->type;
				break;
			case column_STATEMENT:
				return_type = get_sqlvaluetype_from_sqldatatype(
				    column_alias->column->v.column->data_type_struct.data_type, FALSE);
				break;
			default:
				assert(FALSE);
			}
			loop_done = TRUE;
			break;
		case LP_FUNCTION_CALL:
			/* Per plan generation in lp_generate_where and comment in LP_FUNCTION_CALL branch of tmpl_print_expression,
			 * skip over the first two nodes of this plan (function name and function hash) to access the return type.
			 */
			ret_type_plan = cur_plan->v.lp_default.operand[1]->v.lp_default.operand[1]->v.lp_default.operand[1];
			return_type = ret_type_plan->v.lp_default.operand[0]->v.lp_value.value->type;
			assert(BOOLEAN_OR_STRING_LITERAL != return_type);
			assert(!IS_NUL_VALUE(return_type));
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
		case LP_AGGREGATE_FUNCTION_COUNT_ASTERISK:
		case LP_AGGREGATE_FUNCTION_COUNT:
		case LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK:
		case LP_AGGREGATE_FUNCTION_AVG:
		case LP_AGGREGATE_FUNCTION_SUM:
		case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT:
		case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK:
		case LP_AGGREGATE_FUNCTION_AVG_DISTINCT:
		case LP_AGGREGATE_FUNCTION_SUM_DISTINCT:
			loop_done = TRUE;
			break;
		case LP_BOOLEAN_EQUALS:
		case LP_BOOLEAN_OR:
		case LP_BOOLEAN_AND:
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
		case LP_BOOLEAN_IN:
		case LP_BOOLEAN_NOT_IN:
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
			return_type = BOOLEAN_VALUE;
			loop_done = TRUE;
			break;
		default:
			/* Due to the above switch/case blocks, if we reach here, it means this plan type is guaranteed to
			 * have `cur_plan->v.lp_default` usable. This relies on the current layout of the `v` member in the
			 * `LogicalPlan` structure and that none of the LP_* possibilities in the "default:" case here
			 * can possibly have a `v` member usable other than `lp_default`. Any changes to the `v` union
			 * layout might need to be reflected here (LOGICAL_PLAN_KEEP_IN_SYNC).
			 */
			switch (cur_plan->type) {
			case LP_SET_OPERATION:
			case LP_CASE:
			case LP_CASE_BRANCH_STATEMENT:
				/* 1) LP_SET_OPERATION has LP_SET_OPTION as [0] AND LP_PLANS as [1].
				 *    We need to go to [1] to eventually arrive at the LP_COLUMN_LIST and derive the type
				 *    information from it. So this is an exception from the rest which use [0].
				 * 2) Similar reasoning for LP_CASE which has LP_CASE_STATEMENT as [0] and LP_CASE_BRANCH as [1].
				 *    We need to go to [1] to arrive at LP_CASE_BRANCH as LP_CASE_STATEMENT only has the
				 *    case condition and not the branch value (which is needed for the result type).
				 * 3) Similar reasoning for LP_CASE_BRANCH_STATEMENT which has branch condition as [0] and
				 *    branch value as [1]. We need latter to determine the result type.
				 */
				cur_plan = cur_plan->v.lp_default.operand[1];
				break;
			default:
				cur_plan = cur_plan->v.lp_default.operand[0];
				assert(NULL != cur_plan);
				break;
			}
			break;
		}
	}
	return return_type;
}
