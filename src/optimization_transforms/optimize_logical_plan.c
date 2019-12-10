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
 * plan should be a LP_TABLE_JOIN
 */
LogicalPlan *join_tables(LogicalPlan *root, LogicalPlan *plan) {
	LogicalPlan	*next = NULL, *table_plan;
	LogicalPlan	*keys = NULL, *select, *criteria, *cur_lp_key;
	LogicalPlan	*sub;
	SqlTable	*table;
	SqlTableAlias	*table_alias;
	SqlColumn	*key_columns[MAX_KEY_COUNT];
	int		max_key, cur_key, unique_id;

	if ((LP_TABLE_JOIN != plan->type) || (NULL == plan->v.lp_default.operand[0])) {
		return plan;
	}
	sub = plan->v.lp_default.operand[0];
	if (LP_INSERT == plan->v.lp_default.operand[0]->type) {
		// This plan needs to be inserted as a physical plan
		// Leave it alone here, and let the physical planner grab it
		plan->counter = root->counter;
		plan->v.lp_default.operand[0] = sub = optimize_logical_plan(plan->v.lp_default.operand[0]);
		if (NULL == sub)
			return NULL;
		assert(plan->counter == root->counter);
	}
	if (LP_SET_OPERATION == sub->type) {
		LogicalPlan	*set_plans;

		GET_LP(set_plans, sub, 1, LP_PLANS);
		set_plans->v.lp_default.operand[0] = optimize_logical_plan(set_plans->v.lp_default.operand[0]);
		if (NULL == set_plans->v.lp_default.operand[0])
			return NULL;
		set_plans->v.lp_default.operand[1] = optimize_logical_plan(set_plans->v.lp_default.operand[1]);
		if (NULL == set_plans->v.lp_default.operand[1])
			return NULL;
		cur_lp_key = lp_get_output_key(sub);
		lp_insert_key(root, cur_lp_key);
		return plan;
	}
	if (NULL != plan->v.lp_default.operand[1]) {
		GET_LP(next, plan, 1, LP_TABLE_JOIN);
	}
	if (LP_INSERT == plan->v.lp_default.operand[0]->type) {
		// Append that plans output keys to my list of keys
		GET_LP(cur_lp_key, sub, 1, LP_OUTPUT);
		GET_LP(cur_lp_key, cur_lp_key, 0, LP_KEY);
		lp_insert_key(root, cur_lp_key);
		if (next)
			if (NULL == join_tables(root, next))
				return NULL;
		return plan;
	}
	GET_LP(table_plan, plan, 0, LP_TABLE);
	select = lp_get_select(root);
	GET_LP(criteria, select, 1, LP_CRITERIA);
	GET_LP(keys, criteria, 0, LP_KEYS);
	// Drill down to the bottom of the keys
	while (NULL != keys->v.lp_default.operand[1]) {
		GET_LP(keys, keys, 1, LP_KEYS);
	}
	// If we drilled down somewhat, make sure we start on a fresh "key"
	if (NULL != keys->v.lp_default.operand[0]) {
		MALLOC_LP_2ARGS(keys->v.lp_default.operand[1], LP_KEYS);
		keys = keys->v.lp_default.operand[1];
	}
	if (LP_TABLE == table_plan->type) {
		table_alias = table_plan->v.lp_table.table_alias;
		UNPACK_SQL_STATEMENT(table, table_alias->table, table);
		unique_id = table_alias->unique_id;
		memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
		max_key = get_key_columns(table, key_columns);
		for(cur_key = 0; cur_key <= max_key; cur_key++) {
			MALLOC_LP(cur_lp_key, keys->v.lp_default.operand[0], LP_KEY);
			OCTO_CMALLOC_STRUCT(cur_lp_key->v.lp_key.key, SqlKey);
			memset(cur_lp_key->v.lp_key.key, 0, sizeof(SqlKey));
			cur_lp_key->v.lp_key.key->column = key_columns[cur_key];
			cur_lp_key->v.lp_key.key->key_num = cur_key;
			cur_lp_key->v.lp_key.key->unique_id = unique_id;
			cur_lp_key->v.lp_key.key->table = table;
			cur_lp_key->v.lp_key.key->type = LP_KEY_ADVANCE;
			cur_lp_key->v.lp_key.key->owner = table_plan;
			if (cur_key != max_key) {
				MALLOC_LP_2ARGS(keys->v.lp_default.operand[1], LP_KEYS);
				keys = keys->v.lp_default.operand[1];
			}
		}
	} else if (LP_INSERT == table_plan->type) {
		// Else, we read from the output of the previous statement statement as a key
		GET_LP(cur_lp_key, table_plan, 1, LP_KEY);
		keys->v.lp_default.operand[0] = lp_copy_plan(cur_lp_key);
	} else {
		assert(FALSE);
	}
	if (next) {
		if (NULL == join_tables(root, next)) {
			return NULL;
		}
	}
	return plan;
}


LogicalPlan *optimize_logical_plan(LogicalPlan *plan) {
	LogicalPlan	*select, *table_join, *where;
	boolean_t	disable_dnf_expansion;
	LogicalPlan	*new_plan;
	int		num_outer_joins;
	if (NULL == plan)
		return NULL;

	if (LP_SET_OPERATION == plan->type) {
		plan->v.lp_default.operand[1]->v.lp_default.operand[0]
				= optimize_logical_plan(plan->v.lp_default.operand[1]->v.lp_default.operand[0]);
		if (NULL == plan->v.lp_default.operand[1]->v.lp_default.operand[0]) {
			return NULL;
		}
		plan->v.lp_default.operand[1]->v.lp_default.operand[1]
				= optimize_logical_plan(plan->v.lp_default.operand[1]->v.lp_default.operand[1]);
		if (NULL == plan->v.lp_default.operand[1]->v.lp_default.operand[1]) {
			return NULL;
		}
		return plan;
	}
	// First, "join" all the tables; we should do a search here to find the
	//  optimal join order
	select = lp_get_select(plan);
	GET_LP(table_join, select, 0, LP_TABLE_JOIN);
	// We can end coming here with a already populated set of keys if the plan was split for optimization
	// If so, don't add the keys again
	LogicalPlan *keys;
	keys = lp_get_keys(plan);
	if (NULL == keys->v.lp_default.operand[0]) {
		if (NULL == join_tables(plan, table_join)) {
			return NULL;
		}
	}
	// If there are no "OR" or "AND" statements, fix key values
	where = lp_get_select_where(plan);
	new_plan = plan;
	num_outer_joins = where->extra_detail.lp_where.num_outer_joins;
	disable_dnf_expansion = (1 < num_outer_joins);
	if (!disable_dnf_expansion) {
		LogicalPlan	*cur;

		where->v.lp_default.operand[0] = lp_make_normal_disjunctive_form(where->v.lp_default.operand[0]);
		// Expand the plan, if needed
		cur = where->v.lp_default.operand[0];
		while((NULL != cur) && (LP_BOOLEAN_OR == cur->type)) {
			SqlOptionalKeyword	*keywords, *new_keyword;
			LogicalPlan		*p;
			LogicalPlan		*child_where;
			LogicalPlan		*set_operation, *set_option, *set_plans;

			keywords = lp_get_select_keywords(plan)->v.lp_keywords.keywords;
			new_keyword = get_keyword_from_keywords(keywords, OPTIONAL_BOOLEAN_EXPANSION);
			if (NULL == new_keyword) {
				OCTO_CMALLOC_STRUCT(new_keyword, SqlOptionalKeyword);
				dqinit(new_keyword);
				new_keyword->keyword = OPTIONAL_BOOLEAN_EXPANSION;
				dqappend(keywords, new_keyword);
			}
			p = lp_copy_plan(plan);	/* This will store LHS of the LP_BOOLEAN_OR condition */
			child_where = lp_get_select_where(p);
			child_where->v.lp_default.operand[0] = cur->v.lp_default.operand[0];
			MALLOC_LP_2ARGS(set_operation, LP_SET_OPERATION);
			MALLOC_LP(set_option, set_operation->v.lp_default.operand[0], LP_SET_OPTION);
			MALLOC_LP_2ARGS(set_option->v.lp_default.operand[0], LP_SET_DNF);
			MALLOC_LP(set_plans, set_operation->v.lp_default.operand[1], LP_PLANS);
			set_plans->v.lp_default.operand[0] = p;		/* This stores left side of the LP_BOOLEAN_OR condition */
			set_plans->v.lp_default.operand[1] = new_plan;	/* This stores right side of the LP_BOOLEAN_OR condition */
			new_plan = set_operation;
			cur = cur->v.lp_default.operand[1];
		}
		where->v.lp_default.operand[0] = cur;
		if (new_plan != plan) {
			return optimize_logical_plan(new_plan);
		}
	}
	// Perform optimizations where we are able
	do {
		assert(LP_TABLE_JOIN == table_join->type);
		if (NULL != table_join->extra_detail.lp_table_join.join_on_condition) {
			lp_optimize_where_multi_equal_ands(plan, table_join->extra_detail.lp_table_join.join_on_condition);
		}
		table_join = table_join->v.lp_default.operand[1];
	} while (NULL != table_join);
	/* In case an OUTER JOIN exists, we cannot easily optimize the WHERE condition.
	 * Care has to be taken while invoking key fixing optimizations. Will be done at a later time.
	 * Skip it for now.
	 */
	if (!num_outer_joins) {
		lp_optimize_where_multi_equal_ands(plan, where);
	}
	return new_plan;
}
