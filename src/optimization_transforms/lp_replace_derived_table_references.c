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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_replace_helper(LogicalPlan *where, SqlTableAlias *table_alias, SqlKey *key);

/* Goes through various parts of parent query (WHERE, ORDER BY, HAVING clause etc.) pointed to by input parameter `root`
 * and replaces LP_COLUMN_ALIAS references to the input parameter `table_alias` (that corresponds to a sub-query)
 * with a LP_DERIVED_COLUMN and a LP_PIECE_NUMBER/LP_KEY combination.
 */
LogicalPlan *lp_replace_derived_table_references(LogicalPlan *root, SqlTableAlias *table_alias, SqlKey *key) {
	LogicalPlan *t;
	LogicalPlan *table_join;
	LogicalPlan *select, *criteria, *select_options, *select_more_options, *aggregate_options;

	assert(LP_SELECT_QUERY == root->type);
	select = lp_get_select(root);
	GET_LP(criteria, select, 1, LP_CRITERIA);
	GET_LP(select_options, criteria, 1, LP_SELECT_OPTIONS);
	GET_LP(t, select_options, 0, LP_WHERE);
	lp_replace_helper(t, table_alias, key);
	t = lp_get_project(root);
	t->v.lp_default.operand[0] = lp_replace_helper(t->v.lp_default.operand[0], table_alias, key);
	// Make sure to update table references in ORDER BY clause
	t = root->v.lp_default.operand[1];
	assert(LP_OUTPUT == t->type);
	t = t->v.lp_default.operand[1];
	if (NULL != t) {
		assert(LP_ORDER_BY == t->type);
		t->v.lp_default.operand[0] = lp_replace_helper(t->v.lp_default.operand[0], table_alias, key);
		t->v.lp_default.operand[1] = lp_replace_helper(t->v.lp_default.operand[1], table_alias, key);
	}
	// Update table references in FROM clause (i.e. join conditions if they exist)
	table_join = lp_get_table_join(root);
	do {
		if ((NULL != table_join->v.lp_default.operand[0]) && (LP_TABLE != table_join->v.lp_default.operand[0]->type)) {
			/* A query such as the following may need derived table reference replacement in the FROM clause too
			 * SELECT (SELECT * FROM (SELECT n3.id FROM (select * from names) n1) n2) FROM (select * from names) n3;
			 * Hence the lp_replace_helper() call below on the FROM clause.
			 */
			table_join->v.lp_default.operand[0]
			    = lp_replace_helper(table_join->v.lp_default.operand[0], table_alias, key);
		}
		t = table_join->extra_detail.lp_table_join.join_on_condition;
		if (NULL != t)
			t->v.lp_default.operand[0] = lp_replace_helper(t->v.lp_default.operand[0], table_alias, key);
		table_join = table_join->v.lp_default.operand[1];
	} while (NULL != table_join);
	if (NULL != select_options->v.lp_default.operand[1]) {
		GET_LP(select_more_options, select_options, 1, LP_SELECT_MORE_OPTIONS);
		if (NULL != select_more_options->v.lp_default.operand[0]) {
			GET_LP(aggregate_options, select_more_options, 0, LP_AGGREGATE_OPTIONS);
			if (NULL != aggregate_options->v.lp_default.operand[0]) {
				LogicalPlan *group_by;

				// Update derived column references in GROUP BY
				GET_LP(group_by, aggregate_options, 0, LP_GROUP_BY);
				group_by->v.lp_default.operand[0]
				    = lp_replace_helper(group_by->v.lp_default.operand[0], table_alias, key);
			}
			if (NULL != aggregate_options->v.lp_default.operand[1]) {
				LogicalPlan *having;

				// Update derived column references in HAVING
				GET_LP(having, aggregate_options, 1, LP_HAVING);
				having->v.lp_default.operand[0]
				    = lp_replace_helper(having->v.lp_default.operand[0], table_alias, key);
			}
		}
	}
	return root;
}

LogicalPlan *lp_replace_helper(LogicalPlan *plan, SqlTableAlias *table_alias, SqlKey *key) {
	SqlColumnAlias *alias;
	LogicalPlan *	ret, *oper1;
	LogicalPlan *	set_plans;

	if (NULL == plan)
		return NULL;
	ret = plan;
	switch (plan->type) {
	case LP_COLUMN_ALIAS:
		alias = plan->v.lp_column_alias.column_alias;
		if (alias->table_alias_stmt->v.table_alias->unique_id == table_alias->unique_id) {
			int part;

			MALLOC_LP_2ARGS(ret, LP_DERIVED_COLUMN);
			MALLOC_LP_2ARGS(ret->v.lp_default.operand[0], LP_KEY);
			ret->v.lp_default.operand[0]->v.lp_key.key = key;

			if (!is_stmt_table_asterisk(alias->column)) {
				/* Avoid piece information when COUNT with TABLE_ASTERISK is used as we do not need it */
				part = get_column_piece_number(alias, table_alias);
				MALLOC_LP_2ARGS(ret->v.lp_default.operand[1], LP_PIECE_NUMBER);
				ret->v.lp_default.operand[1]->v.lp_piece_number.piece_number = part;
			}
			/* Note down sub-query SqlColumnAlias to later retrieve type information of this column */
			ret->extra_detail.lp_derived_column.subquery_column_alias = alias;
			/* Note down pointer to LP_DERIVED_COLUMN in the LP_COLUMN_ALIAS. This is needed later to generate
			 * correct M code. Note that LP_COLUMN_ALIAS in most places will have been converted to a
			 * LP_DERIVED_COLUMN so the M code generation logic would not need this normally. The only known
			 * exception currently is "key->fixed_to_value". This can point to a LP_COLUMN_ALIAS. It won't point
			 * to a LP_DERIVED_COLUMN (if applicable) because the key fixing optimization happens AFTER the
			 * "lp_replace_derived_table_references()" call. It might be possible to avoid the "derived_column"
			 * field and instead enhance "lp_replace_derived_table_references()" to scan "plan->iterKeys[]"
			 * and examine all the "key->fixed_to_value" fields (some of them could be a list of values) and
			 * invoke "lp_replace_helper()" on those. But this has to be done AFTER "optimize_logical_plan()"
			 * is done with the key fixing. It is not considered worth the effort right now but might be needed
			 * at a later point. Noting the current reasoning here in the hope it will help when the time comes.
			 */
			plan->extra_detail.lp_column_alias.derived_column = ret;
		}
		break;
	case LP_WHERE:
		ret->v.lp_default.operand[0] = lp_replace_helper(plan->v.lp_default.operand[0], table_alias, key);
		oper1 = plan->v.lp_default.operand[1];
		if ((NULL != oper1) && (LP_COLUMN_LIST_ALIAS != oper1->type)) {
			/* Note that it is possible we have `operand[1]` set to a non-NULL value for a LP_WHERE.
			 * In this case, this is the alternate list that "lp_optimize_where_multi_equals_ands_helper()"
			 * built that needs to also be checked for derived table references.
			 */
			ret->v.lp_default.operand[1] = lp_replace_helper(plan->v.lp_default.operand[1], table_alias, key);
		}
		break;
	case LP_SELECT_QUERY:
		lp_replace_derived_table_references(plan, table_alias, key);
		break;
	case LP_SET_OPERATION:
		GET_LP(set_plans, plan, 1, LP_PLANS);
		lp_replace_helper(set_plans->v.lp_default.operand[0], table_alias, key);
		lp_replace_helper(set_plans->v.lp_default.operand[1], table_alias, key);
		break;
	case LP_TABLE_VALUE:
		lp_replace_helper(plan->v.lp_default.operand[0], table_alias, key);
		break;
	case LP_FUNCTION_CALL:
		assert(LP_VALUE == ret->v.lp_default.operand[0]->type);
		ret->v.lp_default.operand[0] = lp_replace_helper(plan->v.lp_default.operand[0], table_alias, key);
		assert(LP_COLUMN_LIST == ret->v.lp_default.operand[1]->type);
		ret->v.lp_default.operand[1] = lp_replace_helper(plan->v.lp_default.operand[1], table_alias, key);
		break;
	case LP_ARRAY:
		// array child can only be one of the following
		assert((LP_SELECT_QUERY == ret->v.lp_default.operand[0]->type)
		       || (LP_TABLE_VALUE == ret->v.lp_default.operand[0]->type)
		       || (LP_SET_OPERATION == ret->v.lp_default.operand[0]->type));
		ret->v.lp_default.operand[0] = lp_replace_helper(plan->v.lp_default.operand[0], table_alias, key);
		break;
	case LP_VALUE:
	case LP_DERIVED_COLUMN:
		// Nothing to do
		break;
	default:
		assert((LP_FUNCTION_CALL <= plan->type) || ((LP_ROW_VALUE == plan->type) || (LP_TABLE_DATA == plan->type)));
		ret->v.lp_default.operand[0] = lp_replace_helper(plan->v.lp_default.operand[0], table_alias, key);
		ret->v.lp_default.operand[1] = lp_replace_helper(plan->v.lp_default.operand[1], table_alias, key);
		break;
	}
	return ret;
}
