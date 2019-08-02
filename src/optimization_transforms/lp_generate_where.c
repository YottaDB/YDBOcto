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

LogicalPlan *lp_generate_where(SqlStatement *stmt, int *plan_id) {
	LogicalPlan *ret = NULL, *next, *cur_lp, *t, *prev;
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
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			ret = lp_generate_where(value->v.calculated, plan_id);
			break;
		default:
			MALLOC_LP_2ARGS(ret, LP_VALUE);
			ret->v.value = value;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		/// WARNING: we simply add the enum offset to find the type
		type = binary->operation + LP_ADDITION;
		// Special case; check for the IN value where the left is a column_list
		if((type == LP_BOOLEAN_IN || type == LP_BOOLEAN_NOT_IN)
				&& binary->operands[1]->type == column_list_STATEMENT) {
			// Walk through the column list, converting each statement to an OR/AND
			UNPACK_SQL_STATEMENT(start_cl, binary->operands[1], column_list);
			t = lp_generate_where(binary->operands[0], plan_id);
			ret = NULL;
			cur_cl = start_cl;
			do {
				if(type == LP_BOOLEAN_IN) {
					MALLOC_LP_2ARGS(next, LP_BOOLEAN_EQUALS);
				} else {
					MALLOC_LP_2ARGS(next, LP_BOOLEAN_NOT_EQUALS);
				}
				next->v.operand[0] = t;
				next->v.operand[1] = lp_generate_where(cur_cl->value, plan_id);
				cur_cl = cur_cl->next;
				if(ret == NULL) {
					ret = next;
				} else {
					prev = ret;
					if(type == LP_BOOLEAN_IN) {
						MALLOC_LP_2ARGS(ret, LP_BOOLEAN_OR);
					} else {
						MALLOC_LP_2ARGS(ret, LP_BOOLEAN_AND);
					}
					ret->v.operand[0] = next;
					ret->v.operand[1] = prev;
				}
			} while(start_cl != cur_cl);
		} else {
			MALLOC_LP_2ARGS(ret, type);
			ret->v.operand[0] = lp_generate_where(binary->operands[0], plan_id);
			ret->v.operand[1] = lp_generate_where(binary->operands[1], plan_id);
			/// OPTIMIZATION: if this is a regex, see if the pattern is entirely characters,
			// and if so, change it to a LP_BOOLEAN_EQUALS so we can optimize it later
			if(ret->type == LP_BOOLEAN_REGEX_SENSITIVE && ret->v.operand[1]->type == LP_VALUE) {
				SqlValue *value = ret->v.operand[1]->v.value;
				if(value->type != STRING_LITERAL && value->type != NUMBER_LITERAL) {
					break;
				}
				char *c = value->v.string_literal;
				if(*c != '^') {
					break;
				}
				c++;
				int done = FALSE;
				while(*c != '\0' && *c != '$' && !done) {
					switch(*c) {
					case '\\':
					case '*':
					case '+':
					case '{':
					case '(':
					case ')':
					case '}':
					case '[':
					case ']':
					case '?':
					case '.':
					case '|':
						done = TRUE;
						break;
					default:
						c++;
						break;
					}
				}
				if(*c != '$') {
					break;
				}
				c++;
				if(*c == '\0') {
					if(value->v.string_literal[0] == '^') {
						value->v.string_literal = value->v.string_literal + 1;
					}
					if(*(c - 1) == '$') {
						*(c - 1) = '\0';
					}
					ret->type = LP_BOOLEAN_EQUALS;
				}
			}
		}
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		/// WARNING: we simply add the enum offset to find the type
		type = unary->operation + LP_FORCE_NUM;
		MALLOC_LP_2ARGS(ret, type);
		ret->v.operand[0] = lp_generate_where(unary->operand, plan_id);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		type = LP_FUNCTION_CALL;
		MALLOC_LP_2ARGS(ret, type);
		ret->v.operand[0] = lp_generate_where(function_call->function_name, plan_id);
		// TODO: we should move the logic to loop through the list to a seperate function and reuse it
		// Too bad this isn't written in Go :'(
		UNPACK_SQL_STATEMENT(start_cl, function_call->parameters, column_list);
		cur_cl = start_cl;
		cur_lp = ret;
		do {
			next = lp_generate_where(cur_cl->value, plan_id);
			if(next != NULL) {
				MALLOC_LP_2ARGS(cur_lp->v.operand[1], LP_COLUMN_LIST);
				cur_lp = cur_lp->v.operand[1];
				cur_lp->v.operand[0] = next;
			}
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case column_alias_STATEMENT:
		MALLOC_LP_2ARGS(ret, LP_COLUMN_ALIAS);
		UNPACK_SQL_STATEMENT(ret->v.column_alias, stmt, column_alias);
		break;
	case cas_STATEMENT:
		MALLOC_LP_2ARGS(ret, LP_CASE);
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		// First put in the default branch, if needed, and value
		MALLOC_LP(cur_lp, ret->v.operand[0], LP_CASE_STATEMENT);
		cur_lp->v.operand[0] = lp_generate_where(cas->value, plan_id);
		if(cas->optional_else != NULL) {
			cur_lp->v.operand[1] = lp_generate_where(cas->optional_else, plan_id);
		}
		UNPACK_SQL_STATEMENT(cas_branch, cas->branches, cas_branch);
		cur_branch = cas_branch;
		MALLOC_LP(cur_lp, ret->v.operand[1], LP_CASE_BRANCH);
		do {
			MALLOC_LP(t, cur_lp->v.operand[0], LP_CASE_BRANCH_STATEMENT);
			t->v.operand[0] = lp_generate_where(cur_branch->condition, plan_id);
			t->v.operand[1] = lp_generate_where(cur_branch->value, plan_id);
			cur_branch = cur_branch->next;
			if(cur_branch != cas_branch) {
				MALLOC_LP_2ARGS(cur_lp->v.operand[1], LP_CASE_BRANCH);
				cur_lp = cur_lp->v.operand[1];
			}
		} while(cur_branch != cas_branch);
		break;
	case set_operation_STATEMENT:
	case table_alias_STATEMENT:
		ret = generate_logical_plan(stmt, plan_id);
		/// TODO: should this be moved to the optimize phase for this plan?
		ret = optimize_logical_plan(ret);
		break;
	case select_STATEMENT:
		// This should never happen, as all select statements are now wrapped in a table_alias
	case table_STATEMENT:
		// In most other cases, we expect that this can be, but not here because a table can't exist
		// in a WHERE statement
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
	}

	return ret;
}
