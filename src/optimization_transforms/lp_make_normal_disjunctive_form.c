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
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/**
 * Applies a NOT operation to all sub-plans if (count % 2) is non-zero (i.e. 1)
 * Effectively, this means switching OR to AND, and inverting
 * < to be >=, > to be <=, = to be <>, and <> to be =
 */
LogicalPlan *lp_apply_not(LogicalPlan *root, int count) {
	LPActionType	type;

	if (LP_BOOLEAN_NOT == root->type) {
		type = root->v.operand[0]->type;
		// Don't recurse for stuff that we cannot apply the NOT operation.
		// (e.g. regex calls, or anything like a function call or columns ref)
		if ((LP_BOOLEAN_REGEX_SENSITIVE == type)
				|| (LP_BOOLEAN_REGEX_INSENSITIVE == type)
				|| (LP_BOOLEAN_IS == type)
				|| (LP_ADDITION > type)) {
			return root;
		}
		count++;
		// This will trim the NOT from the expression
		return lp_apply_not(root->v.operand[0], count);
	}
	if (count % 2) {
		switch(root->type) {
			case LP_BOOLEAN_OR:
				root->type = LP_BOOLEAN_AND;
				root->v.operand[0] = lp_apply_not(root->v.operand[0], count);
				root->v.operand[1] = lp_apply_not(root->v.operand[1], count);
				break;
			case LP_BOOLEAN_AND:
				root->type = LP_BOOLEAN_OR;
				root->v.operand[0] = lp_apply_not(root->v.operand[0], count);
				root->v.operand[1] = lp_apply_not(root->v.operand[1], count);
				break;
			case LP_BOOLEAN_EQUALS:
				root->type = LP_BOOLEAN_NOT_EQUALS;
				break;
			case LP_BOOLEAN_NOT_EQUALS:
				root->type = LP_BOOLEAN_EQUALS;
				break;
			case LP_BOOLEAN_LESS_THAN:
				root->type = LP_BOOLEAN_GREATER_THAN_OR_EQUALS;
				break;
			case LP_BOOLEAN_GREATER_THAN:
				root->type = LP_BOOLEAN_LESS_THAN_OR_EQUALS;
				break;
			case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
				root->type = LP_BOOLEAN_GREATER_THAN;
				break;
			case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
				root->type = LP_BOOLEAN_LESS_THAN;
				break;
			case LP_BOOLEAN_IN:
				root->type = LP_BOOLEAN_NOT_IN;
				break;
			case LP_BOOLEAN_NOT_IN:
				root->type = LP_BOOLEAN_IN;
				break;
			case LP_BOOLEAN_ANY_EQUALS:
				root->type = LP_BOOLEAN_ALL_NOT_EQUALS;
				break;
			case LP_BOOLEAN_ANY_NOT_EQUALS:
				root->type = LP_BOOLEAN_ALL_EQUALS;
				break;
			case LP_BOOLEAN_ANY_LESS_THAN:
				root->type = LP_BOOLEAN_ALL_GREATER_THAN_OR_EQUALS;
				break;
			case LP_BOOLEAN_ANY_GREATER_THAN:
				root->type = LP_BOOLEAN_ALL_LESS_THAN_OR_EQUALS;
				break;
			case LP_BOOLEAN_ANY_LESS_THAN_OR_EQUALS:
				root->type = LP_BOOLEAN_ALL_GREATER_THAN;
				break;
			case LP_BOOLEAN_ANY_GREATER_THAN_OR_EQUALS:
				root->type = LP_BOOLEAN_ALL_LESS_THAN;
				break;
			case LP_BOOLEAN_ALL_EQUALS:
				root->type = LP_BOOLEAN_ANY_NOT_EQUALS;
				break;
			case LP_BOOLEAN_ALL_NOT_EQUALS:
				root->type = LP_BOOLEAN_ANY_EQUALS;
				break;
			case LP_BOOLEAN_ALL_LESS_THAN:
				root->type = LP_BOOLEAN_ANY_GREATER_THAN_OR_EQUALS;
				break;
			case LP_BOOLEAN_ALL_GREATER_THAN:
				root->type = LP_BOOLEAN_ANY_LESS_THAN_OR_EQUALS;
				break;
			case LP_BOOLEAN_ALL_LESS_THAN_OR_EQUALS:
				root->type = LP_BOOLEAN_ANY_GREATER_THAN;
				break;
			case LP_BOOLEAN_ALL_GREATER_THAN_OR_EQUALS:
				root->type = LP_BOOLEAN_ANY_LESS_THAN;
				break;
			case LP_BOOLEAN_EXISTS:
				root->type = LP_BOOLEAN_NOT_EXISTS;
				break;
			default:
				// We should never recurse into anything except boolean values
				assert(FALSE);
				break;
		}
	}
	return root;
}

/**
 * After a call to this function, all logical disjunctions will be on the right, with each left
 * representing a term with only conjunctions and boolean operations
 */
LogicalPlan *lp_make_normal_disjunctive_form(LogicalPlan *root) {
	// TODO: we need to handle NOT
	if (NULL == root)
		return root;
	root = lp_apply_not(root, 0);
	if ((LP_BOOLEAN_AND != root->type) && (LP_BOOLEAN_OR != root->type)) {
		return root;
	}
	LogicalPlan *left = lp_make_normal_disjunctive_form(root->v.operand[0]);
	LogicalPlan *right = lp_make_normal_disjunctive_form(root->v.operand[1]);

	// Algorithm:
	// 1. Process left and right children into normal form
	// 2. For each each clause on the left:
	//  2.1 For each clause on the right:
	//   2.1.1 If clause on left is OR, and clause on right is OR, construct new AND of left and right
	//   2.1.2 If clause of left is AND, and clause on right is OR, join with

	// If this case is not an AND, we don't need to combine things, so return early
	if (LP_BOOLEAN_AND != root->type) {
		root->v.operand[0] = left;
		root->v.operand[1] = right;
		return root;
	}

	// Else, walk through the left list, right list, and form a new series
	LogicalPlan *ret = NULL, *cur = NULL;
	LogicalPlan *l = left;
	do {
		LogicalPlan *r = right;
		do {
			LogicalPlan *lp;
			LPActionType boolean_type = LP_BOOLEAN_OR;
			MALLOC_LP_2ARGS(lp, LP_BOOLEAN_AND);

			LogicalPlan *next_l = l;
			if (LP_BOOLEAN_OR == next_l->type) {
				next_l = next_l->v.operand[0];
			}
			LogicalPlan *next_r = r;
			if (LP_BOOLEAN_OR == next_r->type) {
				next_r = next_r->v.operand[0];
			}
			if ((LP_BOOLEAN_OR != l->type) && (LP_BOOLEAN_OR != r->type)) {
				boolean_type = LP_BOOLEAN_AND;
			}
			lp->v.operand[0] = next_l;
			lp->v.operand[1] = next_r;
			if (NULL == cur) {
				MALLOC_LP_2ARGS(ret, boolean_type);
				cur = ret;
			}
			cur->v.operand[0] = lp;
			MALLOC_LP_2ARGS(cur->v.operand[1], boolean_type);
			cur = cur->v.operand[1];
			if ((LP_BOOLEAN_OR != r->type)) {
				break;
			}
			r = r->v.operand[1];
		} while (r);
		if ((LP_BOOLEAN_OR != l->type)) {
			break;
		}
		l = l->v.operand[1];
	} while (l);

	// Promote the last item
	if ((NULL == ret->v.operand[1]) || (NULL == ret->v.operand[1]->v.operand[0])) {
		ret = ret->v.operand[0];
	} else {
		LogicalPlan	*prev;

		prev = ret;
		cur = ret->v.operand[1];
		while ((NULL != cur->v.operand[1]) && (NULL != cur->v.operand[1]->v.operand[0])) {
			prev = cur;
			cur = cur->v.operand[1];
		}
		prev->v.operand[1] = cur->v.operand[0];
	}
	return ret;
}
