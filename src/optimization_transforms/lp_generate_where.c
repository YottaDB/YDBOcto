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

LogicalPlan *lp_generate_where(SqlStatement *stmt, int *plan_id, SqlStatement *parent) {
	LogicalPlan		*ret = NULL, *next, *cur_lp, *t, *prev;
	LPActionType		type;
	SqlValue		*value;
	SqlUnaryOperation	*unary;
	SqlBinaryOperation	*binary;
	SqlFunctionCall		*function_call;
	SqlColumnList		*cur_cl, *start_cl;
	SqlCaseStatement	*cas;
	SqlCaseBranchStatement	*cas_branch, *cur_branch;
	boolean_t		null_return_seen = FALSE;

	assert(NULL != stmt);
	switch(stmt->type) {
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			LP_GENERATE_WHERE(value->v.calculated, plan_id, stmt, ret, null_return_seen);
			break;
		default:
			MALLOC_LP_2ARGS(ret, LP_VALUE);
			ret->v.value = value;
			break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		/// WARNING: we simply add the enum offset to find the type
		type = binary->operation + LP_ADDITION;
		// Special case; check for the IN value where the left is a column_list
		if (((LP_BOOLEAN_IN == type) || (LP_BOOLEAN_NOT_IN == type))
				&& (column_list_STATEMENT == binary->operands[1]->type))
		{
			// Walk through the column list, converting each statement to an OR/AND
			UNPACK_SQL_STATEMENT(start_cl, binary->operands[1], column_list);
			LP_GENERATE_WHERE(binary->operands[0], plan_id, stmt, t, null_return_seen);
			cur_cl = start_cl;
			do {
				if (LP_BOOLEAN_IN == type) {
					MALLOC_LP_2ARGS(next, LP_BOOLEAN_EQUALS);
				} else {
					MALLOC_LP_2ARGS(next, LP_BOOLEAN_NOT_EQUALS);
				}
				next->v.operand[0] = t;
				LP_GENERATE_WHERE(cur_cl->value, plan_id, stmt, next->v.operand[1], null_return_seen);
				cur_cl = cur_cl->next;
				if (NULL == ret) {
					ret = next;
				} else {
					prev = ret;
					if (LP_BOOLEAN_IN == type) {
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
			LP_GENERATE_WHERE(binary->operands[0], plan_id, stmt, ret->v.operand[0], null_return_seen);
			LP_GENERATE_WHERE(binary->operands[1], plan_id, stmt, ret->v.operand[1], null_return_seen);
			/* OPTIMIZATION: if this is a regex
			 * see if the pattern is entirely characters,
			 * and if so, change it to a LP_BOOLEAN_EQUALS so we can optimize it later
			 *
			 * see if the pattern is entirely .*'s
			 * if so change it to a LP_VALUE->true as that matches everything and doesn't need to go though the engine
			 */
			if (((LP_BOOLEAN_REGEX_SENSITIVE == ret->type) || (LP_BOOLEAN_REGEX_INSENSITIVE == ret->type))
										&& (LP_VALUE == ret->v.operand[1]->type)) {
				SqlValue	*value;
				char		*c;
				boolean_t	done, is_literal;

				value = ret->v.operand[1]->v.value;
				if ((STRING_LITERAL != value->type) && (NUMERIC_LITERAL != value->type)
						&& (INTEGER_LITERAL != value->type)) {
					break;
				}
				c = value->v.string_literal;
				if ('^' == *c) {
					c++;
				}
				/* flags for optimization
				 * loop through the regex once and disable the flags as they are ruled out
				 */
				done = FALSE, is_literal = TRUE;
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
						is_literal = FALSE;
						done = TRUE;
						break;
					default:
						c++;
						break;
					}

				}
				if ('$' == *c) {
					c++;
				}
				/* check that the start and end of the regex is ^$
				 * and is a sensitive regex*/
				if (('$' != *(c - 1)) || ('^' != value->v.string_literal[0])
						|| (LP_BOOLEAN_REGEX_INSENSITIVE == ret->type)) {
					break;
				}
				if ('\0' == *c && is_literal) {
					value->v.string_literal = value->v.string_literal + 1;
					*(c - 1) = '\0';
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
		LP_GENERATE_WHERE(unary->operand, plan_id, stmt, ret->v.operand[0], null_return_seen);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		type = LP_FUNCTION_CALL;
		MALLOC_LP_2ARGS(ret, type);
		LP_GENERATE_WHERE(function_call->function_name, plan_id, stmt, ret->v.operand[0], null_return_seen);
		// TODO: we should move the logic to loop through the list to a seperate function and reuse it
		// Too bad this isn't written in Go :'(
		UNPACK_SQL_STATEMENT(start_cl, function_call->parameters, column_list);
		cur_cl = start_cl;
		cur_lp = ret;
		do {
			LP_GENERATE_WHERE(cur_cl->value, plan_id, stmt, next, null_return_seen);
			if (NULL != next) {
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
		LP_GENERATE_WHERE(cas->value, plan_id, stmt, cur_lp->v.operand[0], null_return_seen);
		if (NULL != cas->optional_else) {
			LP_GENERATE_WHERE(cas->optional_else, plan_id, stmt, cur_lp->v.operand[1], null_return_seen);
		}
		UNPACK_SQL_STATEMENT(cas_branch, cas->branches, cas_branch);
		cur_branch = cas_branch;
		MALLOC_LP(cur_lp, ret->v.operand[1], LP_CASE_BRANCH);
		do {
			MALLOC_LP(t, cur_lp->v.operand[0], LP_CASE_BRANCH_STATEMENT);
			LP_GENERATE_WHERE(cur_branch->condition, plan_id, stmt, t->v.operand[0], null_return_seen);
			LP_GENERATE_WHERE(cur_branch->value, plan_id, stmt, t->v.operand[1], null_return_seen);
			cur_branch = cur_branch->next;
			if (cur_branch != cas_branch) {
				MALLOC_LP_2ARGS(cur_lp->v.operand[1], LP_CASE_BRANCH);
				cur_lp = cur_lp->v.operand[1];
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
		ret = NULL;
		break;
	}
	return (null_return_seen ? NULL : ret);
}
