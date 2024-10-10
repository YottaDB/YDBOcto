/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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
#include "logical_plan.h"

/**
 * plan should be a LP_TABLE_JOIN
 */
LogicalPlan *join_tables(LogicalPlan *root, LogicalPlan *plan) {
	LogicalPlan   *keys, *select, *criteria, *cur_lp_key;
	LogicalPlan   *oper0;
	SqlTable      *table;
	SqlTableAlias *table_alias;
	SqlColumn     *key_columns[MAX_KEY_COUNT];
	int	       max_key, cur_key, unique_id;
	LogicalPlan   *set_plans;

	if ((LP_TABLE_JOIN != plan->type)) {
		return plan;
	}
	assert(NULL != plan->v.lp_default.operand[0]);
	oper0 = plan->v.lp_default.operand[0];
	switch (oper0->type) {
	case LP_SET_OPERATION:
	case LP_SELECT_QUERY:
		if (LP_SET_OPERATION == oper0->type) {
			int i;

			GET_LP(set_plans, oper0, 1, LP_PLANS);
			for (i = 0; i < 2; i++) {
				OPTIMIZE_LOGICAL_PLAN_OUTERMOST_CALL(set_plans->v.lp_default.operand[i], FALSE);
				if (NULL == set_plans->v.lp_default.operand[i]) {
					return NULL;
				}
			}
		} else {
			OPTIMIZE_LOGICAL_PLAN_OUTERMOST_CALL(plan->v.lp_default.operand[0], FALSE);
			oper0 = plan->v.lp_default.operand[0];
			if (NULL == oper0) {
				return NULL;
			}
		}
		cur_lp_key = lp_get_output_key(oper0);
		lp_insert_key(root, cur_lp_key);
		break;
	case LP_VIEW:;
		// View definition is optimized right after its generation, no need to call optimize_logical_plan() on it here
		cur_lp_key = lp_get_output_key(oper0);
		lp_insert_key(root, cur_lp_key);
		break;
	default:
		assert((LP_TABLE == oper0->type) || (LP_TABLE_VALUE == oper0->type));
		select = lp_get_select(root);
		GET_LP(criteria, select, 1, LP_CRITERIA);
		GET_LP(keys, criteria, 0, LP_KEYS);
		// Drill down to the tail end of the linked list of keys that have already been allocated.
		while (NULL != keys->v.lp_default.operand[1]) {
			GET_LP(keys, keys, 1, LP_KEYS);
		}
		// Malloc the keys corresponding to this LP_TABLE/LP_TABLE_VALUE and APPEND them to the end of the linked list.
		if (NULL != keys->v.lp_default.operand[0]) {
			MALLOC_LP_2ARGS(keys->v.lp_default.operand[1], LP_KEYS);
			GET_LP(keys, keys, 1, LP_KEYS);
		}
		table_alias = ((LP_TABLE == oper0->type) ? oper0->v.lp_table.table_alias
							 : oper0->extra_detail.lp_select_query.root_table_alias);
		unique_id = table_alias->unique_id;
		if (create_table_STATEMENT == table_alias->table->type) {
			UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
			memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
			max_key = get_key_columns(table, key_columns);
		} else {
			assert(table_value_STATEMENT == table_alias->table->type);
			/* In the case of a table constructed with the VALUES clause, there is just one primary key
			 * column. That is an internally maintained rowId.
			 */
			max_key = 0;
			key_columns[0] = NULL;
			table = NULL;
		}
		for (cur_key = 0; cur_key <= max_key; cur_key++) {
			keys->v.lp_default.operand[0]
			    = lp_alloc_key(table, key_columns[cur_key], unique_id, LP_KEY_ADVANCE, NULL, FALSE, NULL);
			if (cur_key != max_key) {
				MALLOC_LP_2ARGS(keys->v.lp_default.operand[1], LP_KEYS);
				GET_LP(keys, keys, 1, LP_KEYS);
			}
		}
		break;
	}
	/* See if there are other tables in the JOIN list. If so add keys from those too. */
	LogicalPlan *next;
	GET_LP_ALLOW_NULL(next, plan, 1, LP_TABLE_JOIN);
	if (NULL != next) {
		if (NULL == join_tables(root, next)) {
			return NULL;
		}
	}
	return plan;
}

LogicalPlan *optimize_logical_plan(LogicalPlan *plan, LogicalPlanOptions *options) {
	LogicalPlan *select, *table_join, *where;
	LogicalPlan *cur;

	if (NULL == plan)
		return NULL;

	switch (plan->type) {
	case LP_SET_OPERATION:
		plan->v.lp_default.operand[1]->v.lp_default.operand[0]
		    = optimize_logical_plan(plan->v.lp_default.operand[1]->v.lp_default.operand[0], options);
		if (NULL == plan->v.lp_default.operand[1]->v.lp_default.operand[0]) {
			return NULL;
		}
		plan->v.lp_default.operand[1]->v.lp_default.operand[1]
		    = optimize_logical_plan(plan->v.lp_default.operand[1]->v.lp_default.operand[1], options);
		if (NULL == plan->v.lp_default.operand[1]->v.lp_default.operand[1]) {
			return NULL;
		}
		return plan;
		break;
	case LP_INSERT_INTO:; /* semicolon for empty statement so we can declare variables in case block */
		LogicalPlan *lp_insert_into_options, *lp_insert_into_more_options, *lp_ret;

		/* For an INSERT INTO, we only need to optimize the SELECT query (source of the INSERT INTO) */
		GET_LP(lp_insert_into_options, plan, 1, LP_INSERT_INTO_OPTIONS);
		GET_LP(lp_insert_into_more_options, lp_insert_into_options, 1, LP_INSERT_INTO_MORE_OPTIONS);
		lp_ret = optimize_logical_plan(lp_insert_into_more_options->v.lp_default.operand[0], options);
		if (NULL == lp_ret) {
			return NULL;
		}
		lp_insert_into_more_options->v.lp_default.operand[0] = lp_ret;
		return plan;
		break;
	case LP_TABLE_VALUE:
		/* VALUES clause. Nothing to optimize here. Note that it is possible one or more sub-queries are specified
		 * in the data across multiple rows/columns. "optimize_logical_plan()" would be invoked for those sub-queries
		 * separately as part of "lp_generate_where()" processing.
		 */
		return plan;
	case LP_DELETE_FROM:
	case LP_UPDATE:
		break;
	default:
		assert(LP_SELECT_QUERY == plan->type);
		break;
	}
	assert((LP_SELECT_QUERY == plan->type) || (LP_DELETE_FROM == plan->type) || (LP_UPDATE == plan->type));
	/* First focus on the WHERE clause. Before any key fixing can be done, expand the WHERE clause into disjunctive normal form
	 * (DNF expansion) as that is what enables key fixing.
	 */
	where = lp_get_select_where(plan);
	where->v.lp_default.operand[0] = lp_make_normal_disjunctive_form(where->v.lp_default.operand[0]);
	// Expand the plan, if needed
	cur = where->v.lp_default.operand[0];
	if (NULL != cur) {
		LogicalPlan *new_plan;

		new_plan = plan; /* new_plan will change if DNF expansion happens below */
		if (LP_BOOLEAN_OR == cur->type) {
			/* In case there are N LP_BOOLEAN_OR conditions in the LP_WHERE plan, we are going to invoke
			 * "lp_copy_plan" on it N times and each of them will end up copying the LP_WHERE plan but later
			 * we are going to overwrite it with just 1 out of the N LP_BOOLEAN_OR conditions. So instead of
			 * memory allocated for 1 LP_BOOLEAN_OR condition in each of the N DNF plans, we will end up with
			 * N LP_BOOLEAN_OR conditions allocated for each of the N DNF plans but N-1 of those conditions
			 * unused resulting O(N*N) memory wasted (total memory wasted could be in the order of Gigabytes
			 * if N is in the order of thousands). Avoid this by clearing the LP_WHERE before the DNF expansion.
			 */
			where->v.lp_default.operand[0] = NULL;
			assert(NULL == where->v.lp_default.operand[1]);
		}
		while (LP_BOOLEAN_OR == cur->type) {
			SqlOptionalKeyword *keywords, *new_keyword;
			LogicalPlan	   *p;
			LogicalPlan	   *child_where;
			LogicalPlan	   *set_operation, *set_option, *set_plans;

			keywords = lp_get_select_keywords(plan)->v.lp_keywords.keywords;
			new_keyword = get_keyword_from_keywords(keywords, OPTIONAL_BOOLEAN_EXPANSION);
			if (NULL == new_keyword) {
				OCTO_CMALLOC_STRUCT(new_keyword, SqlOptionalKeyword);
				dqinit(new_keyword);
				new_keyword->keyword = OPTIONAL_BOOLEAN_EXPANSION;
				dqappend(keywords, new_keyword);
			}
			p = lp_copy_plan(plan);
			assert((LP_SELECT_QUERY == p->type) || (LP_DELETE_FROM == p->type) || (LP_UPDATE == plan->type));
			child_where = lp_get_select_where(p);
			/* Below sets the LHS of the LP_BOOLEAN_OR condition as the WHERE clause of one of the DNF plans */
			child_where->v.lp_default.operand[0] = cur->v.lp_default.operand[0];
			assert(NULL == child_where->v.lp_default.operand[1]);
			/* Create LP_SET_OPERATION plan to do the DNF operation on each of the individual plans */
			MALLOC_LP_2ARGS(set_operation, LP_SET_OPERATION);
			MALLOC_LP(set_option, set_operation->v.lp_default.operand[0], LP_SET_OPTION);
			MALLOC_LP_2ARGS(set_option->v.lp_default.operand[0], LP_SET_DNF);
			MALLOC_LP(set_plans, set_operation->v.lp_default.operand[1], LP_PLANS);
			set_plans->v.lp_default.operand[0] = p;	       /* This stores left side of the LP_BOOLEAN_OR condition */
			set_plans->v.lp_default.operand[1] = new_plan; /* This stores right side of the LP_BOOLEAN_OR condition */
			new_plan = set_operation;
			cur = cur->v.lp_default.operand[1];
		}
		if (new_plan != plan) {
			/* Below sets the RHS of the last LP_BOOLEAN_OR condition as the WHERE clause of the last DNF plan */
			assert(NULL != cur);
			where->v.lp_default.operand[0] = cur;
			/* TRUE below disables "lp_optimize_order_by()" call as that could produce output in random order
			 * if ORDER BY is removed due to each DNF plan adding rows in an arbitrary order depending on the
			 * WHERE clause OR operands encountered in that order.
			 */
			OPTIMIZE_LOGICAL_PLAN_OUTERMOST_CALL(new_plan, TRUE);
			return new_plan;
		}
	}
	select = lp_get_select(plan);
	assert(where == lp_get_select_where(plan));
	GET_LP(table_join, select, 0, LP_TABLE_JOIN);
	lp_optimize_cross_join(plan, table_join, where); /* Optimize CROSS JOINs in plan (if any) */
	/* Now that optimal join order has been determined, "join" all the tables to generate keys for the physical plan.
	 * This will be needed by the key fixing optimization which is invoked next.
	 */
	DEBUG_ONLY(LogicalPlan * keys);
	DEBUG_ONLY(keys = lp_get_keys(plan));
	DEBUG_ONLY(assert(NULL == keys->v.lp_default.operand[0]));
	if (NULL == join_tables(plan, table_join)) {
		return NULL;
	}
	/* Assert that the first table in the LP_TABLE_JOIN list always has NO_JOIN as the join type.
	 * Only the right side table in a join should store the corresponding join type. This is true of all join types.
	 */
	assert(NO_JOIN == table_join->extra_detail.lp_table_join.cur_join_type);
	/* Now that DNF expansion has occurred, fix any key values if possible using the ON and/or WHERE clauses */
	/* Perform optimizations of the ON condition in JOINs if any exists. Do this before moving on to the WHERE clause. */
	do {
		assert(LP_TABLE_JOIN == table_join->type);
		if (NULL != table_join->extra_detail.lp_table_join.join_on_condition) {
			LogicalPlan   *select_query, *operand0;
			SqlTableAlias *right_table_alias;

			/* Note that even an INNER JOIN will have a non-NULL join_on_condition if it is preceded by
			 * an OUTER JOIN.
			 */
			operand0 = table_join->v.lp_default.operand[0];
			switch (operand0->type) {
			case LP_VIEW:
			case LP_SELECT_QUERY:
			case LP_SET_OPERATION:
				select_query = operand0;
				if (LP_SET_OPERATION == operand0->type) {
					select_query = lp_drill_to_insert(select_query);
					assert(LP_SELECT_QUERY == select_query->type);
				} else if (LP_VIEW == operand0->type) {
					select_query = operand0->v.lp_default.operand[0];
					assert((LP_SELECT_QUERY == select_query->type) || (LP_TABLE_VALUE == select_query->type)
					       || (LP_SET_OPERATION == select_query->type));
				}
				right_table_alias = select_query->extra_detail.lp_select_query.root_table_alias;
				break;
			default:
				assert((LP_TABLE == operand0->type) || (LP_TABLE_VALUE == operand0->type));
				right_table_alias
				    = ((LP_TABLE == operand0->type) ? operand0->v.lp_table.table_alias
								    : operand0->extra_detail.lp_select_query.root_table_alias);
				break;
			}
			lp_optimize_where_multi_equals_ands(plan, table_join->extra_detail.lp_table_join.join_on_condition,
							    right_table_alias, where->extra_detail.lp_where.num_outer_joins);
		}
		GET_LP_ALLOW_NULL(table_join, table_join, 1, LP_TABLE_JOIN);
	} while (NULL != table_join);
	assert(where == lp_get_select_where(plan));
	/* Pass 3rd parameter as NULL below to indicate this is not an OUTER JOIN ON CLAUSE */
	lp_optimize_where_multi_equals_ands(plan, where, NULL, where->extra_detail.lp_where.num_outer_joins);
	/* Now that keys have been generated in the "join_tables()" call above, see if ORDER BY can be optimized.
	 * Disable ORDER BY optimization in case caller signalled so (e.g. DNF expansion occurred).
	 */
	if (!options->disable_lp_optimize_order_by) {
		lp_optimize_order_by(plan);
	}
	return plan;
}
