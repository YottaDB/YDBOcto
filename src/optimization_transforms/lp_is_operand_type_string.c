/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* When `is_null` is NULL this function can only be used for expressions where operand[0] is non-NULL.
 * Additionally, if operand[1] is non-null, it must have the same type as operand[0].
 * When `is_null` is not NULL then the plan is traversed and if its leaf node has a NUL_VALUE, `*is_null`
 * will be set to TRUE. At present Call with `is_null` arguement is done only from tmpl_print_expression.ctemplate
 * LP_GREATEST/LP_LEAST case.
 */
boolean_t lp_is_operand_type_string(LogicalPlan *plan, boolean_t *is_null) {
	boolean_t	ret, loop_done;
	LogicalPlan *	cur_plan, *ret_type_plan;
	SqlColumnAlias *column_alias;
	SqlValueType	return_type;

	/* When `is_null` is not NULL the call is to process LP_GREATEST/LP_LEAST operands and the operand can
	 * be of any type as it is parsed as a `value_expression`. Hence we only perform assertions in case where
	 * `is_null` doesn't have an argument as the type in such calls are known to be the ones asserted below.
	 */
	if (NULL == is_null) {
		assert((LP_BOOLEAN_LESS_THAN == plan->type) || (LP_BOOLEAN_GREATER_THAN == plan->type)
		       || (LP_BOOLEAN_LESS_THAN_OR_EQUALS == plan->type) || (LP_BOOLEAN_GREATER_THAN_OR_EQUALS == plan->type)
		       || (LP_BOOLEAN_ANY_LESS_THAN == plan->type) || (LP_BOOLEAN_ANY_GREATER_THAN == plan->type)
		       || (LP_BOOLEAN_ANY_LESS_THAN_OR_EQUALS == plan->type)
		       || (LP_BOOLEAN_ANY_GREATER_THAN_OR_EQUALS == plan->type) || (LP_BOOLEAN_ANY_EQUALS == plan->type)
		       || (LP_BOOLEAN_ANY_NOT_EQUALS == plan->type) || (LP_BOOLEAN_ALL_LESS_THAN == plan->type)
		       || (LP_BOOLEAN_ALL_GREATER_THAN == plan->type) || (LP_BOOLEAN_ALL_LESS_THAN_OR_EQUALS == plan->type)
		       || (LP_BOOLEAN_ALL_GREATER_THAN_OR_EQUALS == plan->type) || (LP_BOOLEAN_ALL_EQUALS == plan->type)
		       || (LP_BOOLEAN_ALL_NOT_EQUALS == plan->type) || (LP_NULL_IF == plan->type) || (LP_BOOLEAN_IN == plan->type)
		       || (LP_BOOLEAN_NOT_IN == plan->type) || (LP_BOOLEAN_EQUALS == plan->type)
		       || (LP_BOOLEAN_NOT_EQUALS == plan->type));
	}
	/* We assume all values in this expression have the same type, which should be true due to the matching of types
	 * further up the stack (in `populate_data_type`). Traverse down the left side of the logical plan tree until we get
	 * to a plan node which has only a left child (right hand child is NULL) OR stop traversing if we end up with
	 * determine the type of that node
	 */
	cur_plan = plan;
	ret = FALSE;
	if (NULL != is_null)
		*is_null = FALSE;
	for (loop_done = FALSE; !loop_done;) {
		switch (cur_plan->type) {
		case LP_VALUE:
			assert(BOOLEAN_OR_STRING_LITERAL != cur_plan->v.lp_value.value->type);
			if (STRING_LITERAL == cur_plan->v.lp_value.value->type) {
				ret = TRUE;
			} else if ((NULL != is_null) && IS_NUL_VALUE(cur_plan->v.lp_value.value->type)) {
				*is_null = TRUE;
			}
			loop_done = TRUE;
			break;
		case LP_COERCE_TYPE:
			if (STRING_TYPE == cur_plan->extra_detail.lp_coerce_type.coerce_type.data_type) {
				ret = TRUE;
			}
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
				assert(TABLE_ASTERISK == column_alias->column->v.value->type);
				// Treat table.* as a string.
				ret = TRUE;
				break;
			case column_list_alias_STATEMENT:
				assert(BOOLEAN_OR_STRING_LITERAL != column_alias->column->v.column_list_alias->type);
				if (STRING_LITERAL == column_alias->column->v.column_list_alias->type) {
					ret = TRUE;
				}
				break;
			case column_STATEMENT:
				if (STRING_TYPE == column_alias->column->v.column->data_type_struct.data_type) {
					ret = TRUE;
				}
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
			if (STRING_LITERAL == return_type) {
				ret = TRUE;
			}
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
			assert(FALSE == ret);
			loop_done = TRUE;
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
