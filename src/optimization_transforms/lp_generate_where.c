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

LogicalPlan *lp_generate_where(SqlStatement *stmt, int *plan_id, SqlStatement *parent) {
	LogicalPlan		*ret = NULL, *next, *cur_lp;
	LPActionType		type;
	SqlValue		*value;
	SqlUnaryOperation	*unary;
	SqlBinaryOperation	*binary;
	SqlFunctionCall		*function_call;
	SqlAggregateFunction	*aggregate_function;
	SqlColumnList		*cur_cl, *start_cl;
	SqlCaseStatement	*cas;
	SqlCaseBranchStatement	*cas_branch, *cur_branch;
	boolean_t		error_encountered = FALSE;

	assert(NULL != stmt);
	switch(stmt->type) {
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			LP_GENERATE_WHERE(value->v.calculated, plan_id, stmt, ret, error_encountered);
			break;
		case COERCE_TYPE:
			MALLOC_LP_2ARGS(ret, LP_COERCE_TYPE);
			ret->extra_detail.lp_coerce_type.coerce_type = value->coerced_type;
			ret->extra_detail.lp_coerce_type.pre_coerce_type = value->pre_coerced_type;
			LP_GENERATE_WHERE(value->v.coerce_target, plan_id, stmt, ret->v.lp_default.operand[0], error_encountered);
			break;
		default:
			MALLOC_LP_2ARGS(ret, LP_VALUE);
			ret->v.lp_value.value = value;
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		/// WARNING: we simply add the enum offset to find the type
		type = binary->operation + LP_ADDITION;
		/* Special case: Check for IN usage against a list of values */
		if (((LP_BOOLEAN_IN == type) || (LP_BOOLEAN_NOT_IN == type))
						&& (column_list_STATEMENT == binary->operands[1]->type)) {
			boolean_t	expand;
			LogicalPlan	*t, *prev;

			/* Check if this is a LP_BOOLEAN_IN where the left side is a LP_COLUMN_ALIAS and the right side is a list
			 * of values. If so, expand them to a sequence of LP_BOOLEAN_EQUALS operations that are merged by a
			 * LP_BOOLEAN_OR. This might later help with key fixing in  "optimize_logical_plan()" and
			 * "lp_optimize_where_multi_equal_ands()". Else construct a LP_COLUMN_LIST sequence for right side list.
			 */
			expand = ((LP_BOOLEAN_IN == type) && (column_alias_STATEMENT == binary->operands[0]->type));
			/* Walk through the column list, expanding/converting each right side value as appropriate. */
			UNPACK_SQL_STATEMENT(start_cl, binary->operands[1], column_list);
			LP_GENERATE_WHERE(binary->operands[0], plan_id, stmt, t, error_encountered);
			cur_cl = start_cl;
			do {
				if (expand) {
					if (LP_BOOLEAN_IN == type) {
						MALLOC_LP_2ARGS(next, LP_BOOLEAN_EQUALS);
					} else {
						MALLOC_LP_2ARGS(next, LP_BOOLEAN_NOT_EQUALS);
					}
					next->v.lp_default.operand[0] = t;
					LP_GENERATE_WHERE(cur_cl->value, plan_id, stmt,					\
								next->v.lp_default.operand[1], error_encountered);
					if (NULL == ret) {
						ret = next;
					} else {
						prev = ret;
						if (LP_BOOLEAN_IN == type) {
							MALLOC_LP_2ARGS(ret, LP_BOOLEAN_OR);
						} else {
							MALLOC_LP_2ARGS(ret, LP_BOOLEAN_AND);
						}
						ret->v.lp_default.operand[0] = next;
						ret->v.lp_default.operand[1] = prev;
					}
				} else {
					MALLOC_LP_2ARGS(next, LP_COLUMN_LIST);
					LP_GENERATE_WHERE(cur_cl->value, plan_id, stmt,					\
								next->v.lp_default.operand[0], error_encountered);
					if (NULL == ret) {
						MALLOC_LP_2ARGS(ret, type);
						ret->v.lp_default.operand[0] = t;
						ret->v.lp_default.operand[1] = next;
					} else {
						prev->v.lp_default.operand[1] = next;
					}
					prev = next;
				}
				cur_cl = cur_cl->next;
			} while(start_cl != cur_cl);
		} else {
			MALLOC_LP_2ARGS(ret, type);
			LP_GENERATE_WHERE(binary->operands[0], plan_id, stmt, ret->v.lp_default.operand[0], error_encountered);
			LP_GENERATE_WHERE(binary->operands[1], plan_id, stmt, ret->v.lp_default.operand[1], error_encountered);
		}
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		/// WARNING: we simply add the enum offset to find the type
		type = unary->operation + LP_FORCE_NUM;
		MALLOC_LP_2ARGS(ret, type);
		LP_GENERATE_WHERE(unary->operand, plan_id, stmt, ret->v.lp_default.operand[0], error_encountered);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		type = LP_FUNCTION_CALL;
		MALLOC_LP_2ARGS(ret, type);
		LP_GENERATE_WHERE(function_call->function_name, plan_id, stmt, ret->v.lp_default.operand[0], error_encountered);
		UNPACK_SQL_STATEMENT(start_cl, function_call->parameters, column_list);
		cur_cl = start_cl;
		cur_lp = ret;
		do {
			LP_GENERATE_WHERE(cur_cl->value, plan_id, stmt, next, error_encountered);
			if (NULL != next) {
				MALLOC_LP_2ARGS(cur_lp->v.lp_default.operand[1], LP_COLUMN_LIST);
				cur_lp = cur_lp->v.lp_default.operand[1];
				cur_lp->v.lp_default.operand[0] = next;
			}
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		assert((COUNT_AGGREGATE - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_COUNT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((SUM_AGGREGATE - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_SUM - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AVG_AGGREGATE - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_AVG - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((MIN_AGGREGATE - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_MIN - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((MAX_AGGREGATE - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_MAX - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((COUNT_AGGREGATE_DISTINCT - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_COUNT_DISTINCT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AVG_AGGREGATE_DISTINCT - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_AVG_DISTINCT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((SUM_AGGREGATE_DISTINCT - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_FUNCTION_SUM_DISTINCT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_LAST - COUNT_ASTERISK_AGGREGATE)
			== (LP_AGGREGATE_LAST - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert(COUNT_ASTERISK_AGGREGATE <= aggregate_function->type);
		assert(AGGREGATE_LAST > aggregate_function->type);
		type = LP_AGGREGATE_FUNCTION_COUNT_ASTERISK + (aggregate_function->type - COUNT_ASTERISK_AGGREGATE);
		assert(IS_TYPE_LP_AGGREGATE(type));
		MALLOC_LP_2ARGS(ret, type);
		ret->extra_detail.lp_aggregate_function.param_type = aggregate_function->param_type;
		UNPACK_SQL_STATEMENT(cur_cl, aggregate_function->parameter, column_list);
		assert((NULL != cur_cl->value) || (LP_AGGREGATE_FUNCTION_COUNT_ASTERISK == type));
		assert((NULL == cur_cl->value) || (LP_AGGREGATE_FUNCTION_COUNT_ASTERISK != type));
		if (NULL != cur_cl->value) {
			LP_GENERATE_WHERE(cur_cl->value, plan_id, stmt, next, error_encountered);
			assert(NULL != next);
			cur_lp = ret;
			MALLOC_LP_2ARGS(cur_lp->v.lp_default.operand[0], LP_COLUMN_LIST);
			cur_lp = cur_lp->v.lp_default.operand[0];
			cur_lp->v.lp_default.operand[0] = next;
		}
		break;
	case column_alias_STATEMENT:
		MALLOC_LP_2ARGS(ret, LP_COLUMN_ALIAS);
		UNPACK_SQL_STATEMENT(ret->v.lp_column_alias.column_alias, stmt, column_alias);
		break;
	case cas_STATEMENT:
		MALLOC_LP_2ARGS(ret, LP_CASE);
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		// First put in the default branch, if needed, and value
		MALLOC_LP(cur_lp, ret->v.lp_default.operand[0], LP_CASE_STATEMENT);
		LP_GENERATE_WHERE(cas->value, plan_id, stmt, cur_lp->v.lp_default.operand[0], error_encountered);
		if (NULL != cas->optional_else) {
			LP_GENERATE_WHERE(cas->optional_else, plan_id, stmt, cur_lp->v.lp_default.operand[1], error_encountered);
		}
		UNPACK_SQL_STATEMENT(cas_branch, cas->branches, cas_branch);
		cur_branch = cas_branch;
		MALLOC_LP(cur_lp, ret->v.lp_default.operand[1], LP_CASE_BRANCH);
		do {
			LogicalPlan	*t;

			MALLOC_LP(t, cur_lp->v.lp_default.operand[0], LP_CASE_BRANCH_STATEMENT);
			LP_GENERATE_WHERE(cur_branch->condition, plan_id, stmt, t->v.lp_default.operand[0], error_encountered);
			LP_GENERATE_WHERE(cur_branch->value, plan_id, stmt, t->v.lp_default.operand[1], error_encountered);
			cur_branch = cur_branch->next;
			if (cur_branch != cas_branch) {
				MALLOC_LP_2ARGS(cur_lp->v.lp_default.operand[1], LP_CASE_BRANCH);
				cur_lp = cur_lp->v.lp_default.operand[1];
			}
		} while(cur_branch != cas_branch);
		break;
	case set_operation_STATEMENT:
	case table_alias_STATEMENT:
		ret = generate_logical_plan(stmt, plan_id);
		if (NULL != ret)
		{	/* A sub-query inside of a WHERE expression can return only one column in most cases.
			 * The only exception to it is if the parent is an EXISTS operator. Check accordingly.
			 */
			boolean_t	do_num_cols_check;
			int		num_cols;

			do_num_cols_check = ((unary_STATEMENT != parent->type) || (BOOLEAN_EXISTS != parent->v.unary->operation));
			if (do_num_cols_check) {
				num_cols = lp_get_num_cols_in_select_column_list(ret);
				assert(0 < num_cols);
			}
			if (do_num_cols_check && (1 < num_cols)) {
				// Sub query inside of a WHERE expression can only return one column
				ERROR(ERR_SUBQUERY_ONE_COLUMN, "");
				// Print error context
				yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
				ret = NULL;
			} else {
				// TODO: should this be moved to the optimize phase for this plan?
				ret = optimize_logical_plan(ret);
			}
		}
		break;
	case select_STATEMENT:
		// This should never happen, as all select statements are now wrapped in a table_alias
	case table_STATEMENT:
		// In most other cases, we expect that this can be, but not here because a table can't exist
		// in a WHERE statement
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		assert(FALSE);
		ret = NULL;
		break;
	}
	/* Examine "error_encountered" variable to see if any errors were encountered inside LP_GENERATE_WHERE.
	 * If so, propagate this error back to caller so it can stop logical plan stage (i.e. not proceed to physical plan).
	 */
	return (error_encountered ? NULL : ret);
}
