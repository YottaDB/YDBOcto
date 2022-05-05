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

LogicalPlan *lp_generate_where(SqlStatement *stmt, SqlStatement *parent_stmt) {
	LogicalPlan *		ret = NULL, *cur_lp;
	LPActionType		type;
	SqlArray *		array;
	SqlValue *		value;
	SqlUnaryOperation *	unary;
	SqlBinaryOperation *	binary;
	SqlCoalesceCall *	coalesce_call;
	SqlGreatest *		greatest_call;
	SqlLeast *		least_call;
	SqlNullIf *		null_if;
	SqlFunctionCall *	function_call;
	SqlAggregateFunction *	aggregate_function;
	SqlColumnList *		cur_cl, *start_cl;
	SqlCaseStatement *	cas;
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlStatement *		ret_type, *sql_function_name, *sql_function_hash;
	boolean_t		error_encountered = FALSE;
	SqlDataType		data_type;

	assert(NULL != stmt);
	switch (stmt->type) {
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			LP_GENERATE_WHERE(value->v.calculated, stmt, ret, error_encountered);
			break;
		case COERCE_TYPE:
			MALLOC_LP_2ARGS(ret, LP_COERCE_TYPE);
			ret->extra_detail.lp_coerce_type.coerce_type = value->coerced_type;
			ret->extra_detail.lp_coerce_type.pre_coerce_type = value->pre_coerced_type;
			LP_GENERATE_WHERE(value->v.coerce_target, stmt, ret->v.lp_default.operand[0], error_encountered);
			break;
		default:
			MALLOC_LP_2ARGS(ret, LP_VALUE);
			ret->v.lp_value.value = value;
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		/// WARNING: We simply add the enum offset to find the type
		type = binary->operation + LP_ADDITION;
		/* Special case: Check for IN usage against a list of values */
		if (((LP_BOOLEAN_IN == type) || (LP_BOOLEAN_NOT_IN == type))
		    && (column_list_STATEMENT == binary->operands[1]->type)) {
			LogicalPlan *t;

			/* Walk through the column list, converting each right side value as appropriate. */
			UNPACK_SQL_STATEMENT(start_cl, binary->operands[1], column_list);
			LP_GENERATE_WHERE(binary->operands[0], stmt, t, error_encountered);
			MALLOC_LP_2ARGS(ret, type);
			ret->v.lp_default.operand[0] = t;
			error_encountered |= lp_generate_column_list(&ret->v.lp_default.operand[1], stmt, start_cl);
		} else {
			MALLOC_LP_2ARGS(ret, type);
			LP_GENERATE_WHERE(binary->operands[0], stmt, ret->v.lp_default.operand[0], error_encountered);
			LP_GENERATE_WHERE(binary->operands[1], stmt, ret->v.lp_default.operand[1], error_encountered);
		}
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		// WARNING: We simply add the enum offset to find the type
		type = unary->operation + LP_FORCE_NUM;
		MALLOC_LP_2ARGS(ret, type);
		LP_GENERATE_WHERE(unary->operand, stmt, ret->v.lp_default.operand[0], error_encountered);
		break;
	case array_STATEMENT:
		UNPACK_SQL_STATEMENT(array, stmt, array);
		MALLOC_LP_2ARGS(ret, LP_ARRAY);
		assert(NULL != array->argument);
		LP_GENERATE_WHERE(array->argument, stmt, ret->v.lp_default.operand[0], error_encountered);
		/* Note: the if check is needed in case of an error code path (e.g. ERR_SUBQUERY_ONE_COLUMN) */
		if (NULL != ret->v.lp_default.operand[0]) {
			/* ARRAY() syntax is used to convert single column'd return rows from a subquery to a SQL array.
			 * Accordingly, child plans of an array_STATEMENT/LP_ARRAY should always be one of the following choices.
			 * Note that this LP_SELECT_QUERY/LP_SET_OPERATION/LP_TABLE_VALUE needs to be converted to a SQL array.
			 */
			switch (ret->v.lp_default.operand[0]->type) {
			case LP_SELECT_QUERY:
			case LP_TABLE_VALUE:
				ret->v.lp_default.operand[0]->extra_detail.lp_select_query.to_array = TRUE;
				break;
			case LP_SET_OPERATION:
				ret->v.lp_default.operand[0]->extra_detail.lp_set_operation.to_array = TRUE;
				break;
			default:
				assert(FALSE);
				break;
			}
		}
		break;
	case coalesce_STATEMENT:
		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		UNPACK_SQL_STATEMENT(start_cl, coalesce_call->arguments, column_list);
		MALLOC_LP_2ARGS(ret, LP_COALESCE_CALL);
		/* Walk through the column list, converting each right side value as appropriate. */
		error_encountered |= lp_generate_column_list(&ret->v.lp_default.operand[0], stmt, start_cl);
		break;
	case greatest_STATEMENT:
		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		UNPACK_SQL_STATEMENT(start_cl, greatest_call->arguments, column_list);
		MALLOC_LP_2ARGS(ret, LP_GREATEST);
		/* Walk through the column list, converting each right side value as appropriate. */
		error_encountered |= lp_generate_column_list(&ret->v.lp_default.operand[0], stmt, start_cl);
		break;
	case least_STATEMENT:
		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		UNPACK_SQL_STATEMENT(start_cl, least_call->arguments, column_list);
		MALLOC_LP_2ARGS(ret, LP_LEAST);
		/* Walk through the column list, converting each right side value as appropriate. */
		error_encountered |= lp_generate_column_list(&ret->v.lp_default.operand[0], stmt, start_cl);
		break;
	case null_if_STATEMENT:
		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		MALLOC_LP_2ARGS(ret, LP_NULL_IF);
		LP_GENERATE_WHERE(null_if->left, stmt, ret->v.lp_default.operand[0], error_encountered);
		LP_GENERATE_WHERE(null_if->right, stmt, ret->v.lp_default.operand[1], error_encountered);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		type = LP_FUNCTION_CALL;
		MALLOC_LP_2ARGS(ret, type);
		/* Place the SQL function name first for readability in DEBUG plan output. Note that this means we must skip over it
		 * in tmpl_print_expression.ctemplate to get to the relevant node containing the extrinsic function call.
		 */
		SQL_STATEMENT(sql_function_name, value_STATEMENT);
		MALLOC_STATEMENT(sql_function_name, value, SqlValue);
		sql_function_name->v.value->type = STRING_LITERAL;
		sql_function_name->v.value->v.string_literal
		    = function_call->function_schema->v.create_function->function_name->v.value->v.string_literal;
		LP_GENERATE_WHERE(sql_function_name, stmt, ret->v.lp_default.operand[0], error_encountered);

		/* Use an LP_COLUMN_LIST to store the LP_VALUEs used for the function's return type, extrinsic function name, and
		 * identifying hash.
		 */
		MALLOC_LP_2ARGS(ret->v.lp_default.operand[1], LP_COLUMN_LIST);
		cur_lp = ret->v.lp_default.operand[1];
		SQL_STATEMENT(sql_function_hash, value_STATEMENT);
		MALLOC_STATEMENT(sql_function_hash, value, SqlValue);
		sql_function_hash->v.value->type = STRING_LITERAL;
		sql_function_hash->v.value->v.string_literal
		    = function_call->function_schema->v.create_function->function_hash->v.value->v.string_literal;
		// Add the function's hash identified to the plan
		LP_GENERATE_WHERE(function_call->function_schema->v.create_function->function_hash, stmt,
				  cur_lp->v.lp_default.operand[0], error_encountered);
		MALLOC_LP_2ARGS(cur_lp->v.lp_default.operand[1], LP_COLUMN_LIST);
		cur_lp = cur_lp->v.lp_default.operand[1];
		// Add the function's extrinsic function name to the plan
		LP_GENERATE_WHERE(function_call->function_schema->v.create_function->extrinsic_function, stmt,
				  cur_lp->v.lp_default.operand[0], error_encountered);
		// Add the function's return type to the plan
		MALLOC_LP_2ARGS(cur_lp->v.lp_default.operand[1], LP_COLUMN_LIST);
		cur_lp = cur_lp->v.lp_default.operand[1];
		SQL_STATEMENT(ret_type, value_STATEMENT);
		MALLOC_STATEMENT(ret_type, value, SqlValue);
		data_type = function_call->function_schema->v.create_function->return_type->v.data_type_struct.data_type;
		ret_type->v.value->type = get_sqlvaluetype_from_sqldatatype(data_type, FALSE);
		ret_type->v.value->v.string_literal = get_user_visible_type_string(ret_type->v.value->type);
		LP_GENERATE_WHERE(ret_type, stmt, cur_lp->v.lp_default.operand[0], error_encountered);

		UNPACK_SQL_STATEMENT(start_cl, function_call->parameters, column_list);
		// if there are no parameters, no need to walk the list
		if (NULL != start_cl->value) {
			error_encountered |= lp_generate_column_list(&cur_lp->v.lp_default.operand[1], stmt, start_cl);
		}
		break;
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		assert((AGGREGATE_COUNT - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_COUNT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_SUM - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_SUM - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_AVG - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_AVG - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_MIN - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_MIN - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_MAX - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_MAX - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_COUNT_DISTINCT - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_COUNT_DISTINCT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_SUM_DISTINCT - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_SUM_DISTINCT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_AVG_DISTINCT - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_AVG_DISTINCT - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_COUNT_DISTINCT_TABLE_ASTERISK - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_COUNT_TABLE_ASTERISK - AGGREGATE_COUNT_ASTERISK)
		       == (LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert((AGGREGATE_LAST - AGGREGATE_COUNT_ASTERISK) == (LP_AGGREGATE_LAST - LP_AGGREGATE_FUNCTION_COUNT_ASTERISK));
		assert(AGGREGATE_COUNT_ASTERISK <= aggregate_function->type);
		assert(AGGREGATE_LAST > aggregate_function->type);
		type = LP_AGGREGATE_FUNCTION_COUNT_ASTERISK + (aggregate_function->type - AGGREGATE_COUNT_ASTERISK);
		assert(IS_TYPE_LP_AGGREGATE(type));
		MALLOC_LP_2ARGS(ret, type);
		ret->extra_detail.lp_aggregate_function.param_type = aggregate_function->param_type;
		UNPACK_SQL_STATEMENT(cur_cl, aggregate_function->parameter, column_list);
		assert((NULL != cur_cl->value) || (LP_AGGREGATE_FUNCTION_COUNT_ASTERISK == type));
		assert((NULL == cur_cl->value) || (LP_AGGREGATE_FUNCTION_COUNT_ASTERISK != type));
		if (NULL != cur_cl->value) {
			error_encountered |= lp_generate_column_list(&ret->v.lp_default.operand[0], stmt, cur_cl);
		}
		break;
	case column_STATEMENT:
		/* Currently this is reachable only if called for the target columns of an INSERT INTO or UPDATE.
		 * In that case, "generate_logical_plan()" would have passed in "NULL" as the "parent_stmt" parameter.
		 * Assert that.
		 */
		assert(NULL == parent_stmt);
		MALLOC_LP_2ARGS(ret, LP_COLUMN);
		UNPACK_SQL_STATEMENT(ret->v.lp_column.column, stmt, column);
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
		LP_GENERATE_WHERE(cas->value, stmt, cur_lp->v.lp_default.operand[0], error_encountered);
		if (NULL != cas->optional_else) {
			LP_GENERATE_WHERE(cas->optional_else, stmt, cur_lp->v.lp_default.operand[1], error_encountered);
		}
		UNPACK_SQL_STATEMENT(cas_branch, cas->branches, cas_branch);
		cur_branch = cas_branch;
		MALLOC_LP(cur_lp, ret->v.lp_default.operand[1], LP_CASE_BRANCH);
		do {
			LogicalPlan *t;

			MALLOC_LP(t, cur_lp->v.lp_default.operand[0], LP_CASE_BRANCH_STATEMENT);
			LP_GENERATE_WHERE(cur_branch->condition, stmt, t->v.lp_default.operand[0], error_encountered);
			LP_GENERATE_WHERE(cur_branch->value, stmt, t->v.lp_default.operand[1], error_encountered);
			cur_branch = cur_branch->next;
			if (cur_branch != cas_branch) {
				MALLOC_LP_2ARGS(cur_lp->v.lp_default.operand[1], LP_CASE_BRANCH);
				cur_lp = cur_lp->v.lp_default.operand[1];
			}
		} while (cur_branch != cas_branch);
		break;
	case set_operation_STATEMENT:
	case table_alias_STATEMENT:
		ret = generate_logical_plan(stmt);
		if (NULL != ret) { /* A sub-query inside of a WHERE expression can return only one column in most cases.
				    * The only exception to it is if the "parent_stmt" is an EXISTS operator. Check accordingly.
				    */
			boolean_t do_num_cols_check;
			int	  num_cols;

			do_num_cols_check
			    = ((unary_STATEMENT != parent_stmt->type) || (BOOLEAN_EXISTS != parent_stmt->v.unary->operation));
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
				ret = optimize_logical_plan(ret);
			}
		}
		break;
	case select_STATEMENT:
		// This should never happen, as all select statements are now wrapped in a table_alias
	case create_table_STATEMENT:
	case table_value_STATEMENT:
		// In most other cases, we expect that this can be, but not here because a table can't exist
		// in a WHERE/HAVING/ON clause. Only a column reference that links to a table can exist.
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
