/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_generate_where(SqlStatement *stmt, int *plan_id) {
	LogicalPlan *ret = NULL, *next, *cur_lp, *t;
	LPActionType type;
	SqlValue *value;
	SqlUnaryOperation *unary;
	SqlBinaryOperation *binary;
	SqlFunctionCall *function_call;
	SqlColumnList *cur_cl, *start_cl;
	SqlCaseStatement *cas;
	SqlCaseBranchStatement *cas_branch, *cur_branch;

	if(stmt == NULL)
		return NULL;

	switch(stmt->type) {
	case select_STATEMENT:
		ret = generate_logical_plan(stmt, plan_id);
		/// TODO: should this be moved to the optimize phase for this plan?
		ret = optimize_logical_plan(ret);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			ret = lp_generate_where(value->v.calculated, plan_id);
			break;
		default:
			MALLOC_LP(ret, LP_VALUE);
			ret->v.value = value;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		/// WARNING: we simply add the enum offset to find the type
		type = binary->operation + LP_ADDITION;
		MALLOC_LP(ret, type);
		ret->v.operand[0] = lp_generate_where(binary->operands[0], plan_id);
		ret->v.operand[1] = lp_generate_where(binary->operands[1], plan_id);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		/// WARNING: we simply add the enum offset to find the type
		type = unary->operation + LP_FORCE_NUM;
		MALLOC_LP(ret, type);
		ret->v.operand[0] = lp_generate_where(unary->operand, plan_id);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		type = LP_FUNCTION_CALL;
		MALLOC_LP(ret, type);
		ret->v.operand[0] = lp_generate_where(function_call->function_name, plan_id);
		// TODO: we should move the logic to loop through the list to a seperate function and reuse it
		// Too bad this isn't written in Go :'(
		UNPACK_SQL_STATEMENT(start_cl, function_call->parameters, column_list);
		cur_cl = start_cl;
		cur_lp = ret;
		do {
			next = lp_generate_where(cur_cl->value, plan_id);
			if(next != NULL) {
				MALLOC_LP(cur_lp->v.operand[1], LP_COLUMN_LIST);
				cur_lp = cur_lp->v.operand[1];
				cur_lp->v.operand[0] = next;
			}
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case column_alias_STATEMENT:
		MALLOC_LP(ret, LP_COLUMN_ALIAS);
		// If this column_alias referes to a select statement, replace it with
		//  LP_DERIVED_COLUMN
		UNPACK_SQL_STATEMENT(ret->v.column_alias, stmt, column_alias);
		//ret->v.column_alias = stmt->v.column_alias;
		break;
	case cas_STATEMENT:
		MALLOC_LP(ret, LP_CASE);
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		// First put in the default branch, if needed, and value
		cur_lp = MALLOC_LP(ret->v.operand[0], LP_CASE_STATEMENT);
		cur_lp->v.operand[0] = lp_generate_where(cas->value, plan_id);
		if(cas->optional_else != NULL) {
			cur_lp->v.operand[1] = lp_generate_where(cas->optional_else, plan_id);
		}
		UNPACK_SQL_STATEMENT(cas_branch, cas->branches, cas_branch);
		cur_branch = cas_branch;
		cur_lp = MALLOC_LP(ret->v.operand[1], LP_CASE_BRANCH);
		do {
			t = MALLOC_LP(cur_lp->v.operand[0], LP_CASE_BRANCH_STATEMENT);
			t->v.operand[0] = lp_generate_where(cur_branch->condition, plan_id);
			t->v.operand[1] = lp_generate_where(cur_branch->value, plan_id);
			cur_branch = cur_branch->next;
			if(cur_branch != cas_branch) {
				MALLOC_LP(cur_lp->v.operand[1], LP_CASE_BRANCH);
				cur_lp = cur_lp->v.operand[1];
			}
		} while(cur_branch != cas_branch);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
	}

	return ret;
}
