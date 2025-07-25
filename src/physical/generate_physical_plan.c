/****************************************************************
 *								*
 * Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.	*
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
#include "physical_plan.h"
#include "lp_verify_structure.h"

#include "template_helpers.h"

void	     gen_source_keys(PhysicalPlan *out, LogicalPlan *plan);
int	     iterate_keys(PhysicalPlan *out, LogicalPlan *plan);
LogicalPlan *sub_query_check_and_generate_physical_plan(PhysicalPlanOptions *options, LogicalPlan *stmt, LogicalPlan *parent);

PhysicalPlan *generate_physical_plan(LogicalPlan *plan, PhysicalPlanOptions *options) {
	SqlOptionalKeyword *keyword;
	LogicalPlan	   *keys, *table_joins, *select;
	LogicalPlan	   *project, *select_column_list;
	LogicalPlan	   *criteria, *select_options, *select_more_options;
	LogicalPlan	   *where;
	LogicalPlan	   *set_option, *set_plans, *set_output, *set_key;
	PhysicalPlan	   *out, *prev = NULL, *pp_from_lp;
	LPActionType	    set_oper_type, type;
	PhysicalPlanOptions plan_options;
	boolean_t	    is_set_dnf;

	plan_options = *options;
	if (LP_VIEW == plan->type) {
		/* Generate a physical plan for the view definition and store the output key of LP_VIEW->LP_OUTPUT in the
		 * physical plan. This will be later used to emit M code for the view.
		 * Note that there can be many views referring to the same view definition. In this case, LP_OUTPUT key from
		 * all the views are stored in the physical plan of the view definition.
		 */
		out = generate_physical_plan(plan->v.lp_default.operand[0], &plan_options);
		assert((NULL != out) && (NULL != out->lp_select_query));
#ifndef NDEBUG
		// Following assert ensures that an already processed LP_VIEW is not seen
		assert(NULL == plan->extra_detail.lp_view.physical_plan);
		plan->extra_detail.lp_view.physical_plan = out;
#endif
		// Save output key of LP_VIEW in the physical plan
		LogicalPlan *view_key = lp_get_output_key(plan);
		if (MAX_KEY_COUNT > out->view_total_iter_keys) {
			out->viewKeys[out->view_total_iter_keys] = view_key->v.lp_key.key;
			out->view_total_iter_keys++;
			return out;
		} else {
			// MAX_KEY_COUNT <= out->total_iter_keys
			ERROR(ERR_TOO_MANY_SELECT_KEYCOLS, out->view_total_iter_keys, MAX_KEY_COUNT);
			return NULL;
		}

	} else if (LP_SET_OPERATION == plan->type) {
		if (NULL != plan->extra_detail.lp_set_operation.physical_plan) {
			/* This set operation has already been allocated. But do call generate_physical_plan() on the left and
			 * right branch so that dependent plans are ordered correctly.
			 */
			pp_from_lp = plan->extra_detail.lp_set_operation.physical_plan;
			GET_LP(set_option, plan, 0, LP_SET_OPTION);
			GET_LP(set_plans, plan, 1, LP_PLANS);
			out = generate_physical_plan(set_plans->v.lp_default.operand[1], &plan_options);
			assert(out == pp_from_lp);
			UNUSED(out); // Avoid [clang-analyzer-deadcode.DeadStores] warning
			/* Following invocation is required for the correctness of following type of queries
			 *	 create view v as select * from names where lastname = 'Cool' AND
			 *		(firstname = 'Zero' OR lastname = 'Burn');
			 * 	 create view v1 as select * from v union all select * from v;
			 *	 create view v2 as select * from v1 union all select * from v1;
			 *	 select * from v2;
			 */
			out = generate_physical_plan(set_plans->v.lp_default.operand[0], &plan_options);
			UNUSED(out); // Avoid [clang-analyzer-deadcode.DeadStores] warning
			return pp_from_lp;
		}
		// If this is a union plan, construct physical plans for the two children
		PhysicalPlan *next;

		GET_LP(set_option, plan, 0, LP_SET_OPTION);
		GET_LP(set_plans, plan, 1, LP_PLANS);
		out = generate_physical_plan(set_plans->v.lp_default.operand[1], &plan_options);
		if (NULL == out) {
			return NULL;
		}
		assert(NULL != out->lp_select_query);
		set_oper_type = set_option->v.lp_default.operand[0]->type;
		assert((LP_SET_UNION == set_oper_type) || (LP_SET_UNION_ALL == set_oper_type) || (LP_SET_DNF == set_oper_type)
		       || (LP_SET_EXCEPT == set_oper_type) || (LP_SET_EXCEPT_ALL == set_oper_type)
		       || (LP_SET_INTERSECT == set_oper_type) || (LP_SET_INTERSECT_ALL == set_oper_type));
		is_set_dnf = (LP_SET_DNF == set_oper_type);
		if (is_set_dnf) {
			PhysicalPlan *tmp;
			tmp = out;
			if (LP_SET_OPERATION == set_plans->v.lp_default.operand[1]->type) {
				/* Need to get the left most DNF sibling of out */
				assert(NULL != tmp->dnf_prev);
				do {
					next = tmp;
					tmp = next->dnf_prev;
				} while (NULL != tmp);
			} else {
				next = out;
			}
			plan_options.dnf_plan_next = next; /* this helps set prev->dnf_next inside the below nested call */
		}
		plan->extra_detail.lp_set_operation.physical_plan = out;
		prev = generate_physical_plan(set_plans->v.lp_default.operand[0], &plan_options);
		if (NULL == prev) {
			return NULL;
		}
		assert(NULL != prev->lp_select_query);
		if (!is_set_dnf) {
			int	     input_id1, input_id2;
			SetOperType *set_oper, *prev_oper, *out_oper, *next_oper;

			OCTO_CMALLOC_STRUCT(set_oper, SetOperType);
			prev_oper = prev->set_oper_list;
			input_id1 = (NULL == prev_oper) ? prev->outputKey->unique_id : prev_oper->output_id;
			assert(input_id1 != out->outputKey->unique_id);
			out_oper = out->set_oper_list;
			input_id2 = (NULL == out_oper) ? out->outputKey->unique_id : out_oper->output_id;
			set_oper->set_oper_type = set_oper_type;
			set_oper->lp_set_operation = plan;

			/* Connect the LP_SET_OPERATION logical plan to the "set_oper" and corresponding physical plan fields
			 * for later use in "tmpl_invoke_deferred_plan_setoper()".
			 */
			plan->extra_detail.lp_set_operation.set_oper = set_oper;

			set_oper->input_id1 = input_id1;
			set_oper->input_id2 = input_id2;
			GET_LP(set_output, set_option, 1, LP_OUTPUT);
			GET_LP(set_key, set_output, 0, LP_KEY);
			set_oper->output_id = set_key->v.lp_key.key->unique_id;
			set_oper->prev = NULL;
			next_oper = out->set_oper_list;
			set_oper->next = next_oper;
			if (NULL != next_oper) {
				next_oper->prev = set_oper;
			}
			out->set_oper_list = set_oper;
			/* Initializes "is_deferred_plan" field in LP_SET_OPERATION logical plan for later use in
			 * deferred plan generation.
			 */
			boolean_t is_deferred_plan = FALSE;
			int	  i;
			assert(FALSE == plan->extra_detail.lp_set_operation.is_deferred_plan_valid);
			for (i = 0; i < 2; i++) {
				LogicalPlan *set_operand;
				set_operand = set_plans->v.lp_default.operand[i];
				if (LP_SET_OPERATION != set_operand->type) {
					PhysicalPlan *tmp_pplan;
					assert((LP_SELECT_QUERY == set_operand->type) || (LP_SET_OPERATION == set_operand->type)
					       || (LP_TABLE_VALUE == set_operand->type));
					tmp_pplan = set_operand->extra_detail.lp_select_query.physical_plan;
					if (tmp_pplan->is_deferred_plan) {
						is_deferred_plan = TRUE;
					}
				} else {
					LogicalPlan *tmp_set_option;
					GET_LP(tmp_set_option, set_operand, 0, LP_SET_OPTION);

					LPActionType tmp_set_oper_type;
					tmp_set_oper_type = tmp_set_option->v.lp_default.operand[0]->type;
					if (LP_SET_DNF == tmp_set_oper_type) {
						/* In case of a DNF expanded plan, we can obtain the deferred plan status
						 * from any of the DNF sibling plans. Choose the first.
						 */
						LogicalPlan *tmp_plan;
						tmp_plan = lp_drill_to_insert(set_operand);
						assert((LP_SELECT_QUERY == tmp_plan->type) || (LP_TABLE_VALUE == tmp_plan->type));

						PhysicalPlan *tmp_pplan;
						tmp_pplan = tmp_plan->extra_detail.lp_select_query.physical_plan;
						if (tmp_pplan->is_deferred_plan) {
							is_deferred_plan = TRUE;
						}
					} else {
						assert(TRUE == set_operand->extra_detail.lp_set_operation.is_deferred_plan_valid);
						if (set_operand->extra_detail.lp_set_operation.is_deferred_plan) {
							is_deferred_plan = TRUE;
						}
					}
				}
			}
			plan->extra_detail.lp_set_operation.is_deferred_plan = is_deferred_plan;
			plan->extra_detail.lp_set_operation.is_deferred_plan_valid = TRUE;
		} else {
			/* Check if "prev" is a deferred plan and "next" is not. If so fix "next" and all of its right
			 * DNF siblings to be a deferred plan. Similarly, check if "next" is a deferred plan and "prev"
			 * is not. If so, fix "prev" and its left DNF siblings to be a deferred plan. This way all DNF
			 * sibling plans get treated the same way (all of them are either deferred or all of them are not
			 * deferred) and work correctly even if it is not optimal (see YDBOcto#727).
			 */
			assert(next == prev->dnf_next);
			assert(prev == next->dnf_prev);
			if (prev->is_deferred_plan && !next->is_deferred_plan) {
				PhysicalPlan *tmp;
				tmp = next;
				do {
					tmp->is_deferred_plan = TRUE;
					tmp = tmp->dnf_next;
				} while (NULL != tmp);
			} else if (!prev->is_deferred_plan && next->is_deferred_plan) {
				PhysicalPlan *tmp;
				tmp = prev;
				do {
					tmp->is_deferred_plan = TRUE;
					tmp = tmp->dnf_prev;
				} while (NULL != tmp);
			}
			assert(NULL == plan->extra_detail.lp_set_operation.set_oper); /* should be NULL for LP_SET_DNF case */
		}
		return out;
	}
	assert((LP_SELECT_QUERY == plan->type) || (LP_TABLE_VALUE == plan->type) || (LP_INSERT_INTO == plan->type)
	       || (LP_DELETE_FROM == plan->type) || (LP_UPDATE == plan->type));
	/* Note that it is possible a physical plan has already been generated for this logical plan.
	 *
	 * For example "select * from names where EXISTS (select * from names) and (id < 3 or id > 4);" would get expanded as
	 *	"select * from names where EXISTS (select * from names) and (id < 3)"
	 *		OR
	 *	"select * from names where EXISTS (select * from names) and (id > 4)"
	 * The sub-query corresponding to "EXISTS (select * from names)" shows up twice and so "generate_physical_plan()"
	 * would be invoked twice for the same logical plan. But we generate a physical plan only once. We do this by noting
	 * down the physical plan in the logical plan the first time around and skipping physical plan generation during
	 * the second call. There is some reordering of physical plans that is needed (see "dependent_plans_end" usage in
	 * the "else" block below) to ensure plans that are being relied upon are emitted ahead of plans that rely on them.
	 */
	pp_from_lp = plan->extra_detail.lp_select_query.physical_plan;
	if (NULL != pp_from_lp) {
		/* We already generated a physical plan for this logical plan. A lot of other unrelated physical plans might
		 * have been generated since then. We need to move that prior physical plan along with any plans that the prior
		 * plan depends on to the start of the physical plan linked list that way these dependent physical plans get
		 * emitted BEFORE any other depending physical plans that rely on them (would have been generated by caller
		 * of this function).
		 */
		assert((LP_SELECT_QUERY == plan->type) || (LP_TABLE_VALUE == plan->type));

		PhysicalPlan *end_pplan;
		end_pplan = pp_from_lp->dependent_plans_end;
		assert(NULL != end_pplan);

		PhysicalPlan *first_pplan;
		first_pplan = *plan_options.last_plan;
		assert(NULL != first_pplan);
		if (first_pplan == end_pplan) {
			/* Note: This case is only possible when views are involved. Following queries demonstrate the need for this
			 *	 code block.
			 * 	 	create view v1 as select * from names n1 where exists (select * from names n2) and
			 *			(n1.id < 3 or n1.id > 3);
			 *	 	select * from v1 n1 where exists (select * from v1 n2) and (n1.id < 3 or n1.id > 3);
			 *	 and
			 *	 	create view v1 as select firstname,count(id) as aggr from names group by firstname;
			 *		 select * from v1,v1 as n2;
			 */
			/* If parent exists and this plan is after the parent we need to move it such that it comes previous to the
			 * parent. This ensures parent plan is executed after this plan.
			 */
			if (NULL != plan_options.parent) {
				PhysicalPlan *node = pp_from_lp;
				while ((NULL != node) && (node != plan_options.parent)) {
					node = node->next;
				}
				if (NULL != node) {
					// Nothing to do, as this plan already exists previous to the parent plan
					return pp_from_lp;
				} else {
					/* Move the list of nodes that begin from pp_from_lp and end at parent in a way
					 * such that this sublist of nodes come previous to the parent. This way all the nodes
					 * which are are required by pp_from_lp are moved previous to the parent
					 * without disturbing the dependency list of pp_from_lp.
					 * Following queries demonstrate the need for this type of ordering:
					 * 	CREATE VIEW k_306 as SELECT 1 as id;
					 * 	CREATE VIEW k_306d as SELECT * FROM k_306;
					 * 	SELECT 1 FROM k_306 WHERE EXISTS (SELECT * FROM k_306d) AND (1 < 3 OR 1 > 3);
					 * Refer to https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1452192920
					 * for a diagram describing the issue.
					 */
					PhysicalPlan *parent_node = plan_options.parent;
					node = pp_from_lp;
					while ((node != pp_from_lp->dependent_plans_end) && (node->prev != parent_node)) {
						node = node->prev;
					}
					assert(NULL != node);
					// The sublist can have one or more nodes
					PhysicalPlan *node_to_move_end = node;
					PhysicalPlan *node_to_move_start = pp_from_lp;
					if (NULL == node_to_move_start->next) {
						parent_node->next = NULL;
					} else {
						node_to_move_start->next->prev = parent_node;
						parent_node->next = node_to_move_start->next;
					}
					if (NULL == parent_node->prev) {
						node_to_move_end->prev = NULL;
					} else {
						node_to_move_end->prev = parent_node->prev;
						node_to_move_end->prev->next = node_to_move_end;
					}
					parent_node->prev = node_to_move_start;
					node_to_move_start->next = parent_node;
				}
			}
			return pp_from_lp;
		}
		assert(first_pplan != end_pplan);
#ifndef NDEBUG
		PhysicalPlan *tmp_pplan;
		tmp_pplan = end_pplan;
		for (; tmp_pplan != pp_from_lp; tmp_pplan = tmp_pplan->next) {
			;
		}
#endif
		end_pplan->prev->next = pp_from_lp->next;
		pp_from_lp->next->prev = end_pplan->prev;
		end_pplan->prev = NULL;
		pp_from_lp->next = first_pplan;
		first_pplan->prev = pp_from_lp;
		*plan_options.last_plan = end_pplan;
		return pp_from_lp;
	}
	/* Do logical plan verification once more. But this time overload it to also do the following.
	 *	1) Fix aggregate function counts.
	 *	2) Fix plan_options.table to point to list of LP_TABLE plans (caller emit_sql_statement relies on this).
	 *	3) Fix plan_options.function to point to list of LP_FUNCTION_CALL plans (caller emit_sql_statement relies on this).
	 *	4) Set plan_options.regexmatch_invoked if any LP_BOOLEAN_REGEX_* child plans are encountered.
	 * We achieve this by passing a non-NULL second parameter (first call to previously would have passed a NULL parameter).
	 */
	assert(NULL == plan->extra_detail.lp_select_query.first_aggregate);
	plan_options.aggregate = &plan->extra_detail.lp_select_query.first_aggregate;
	plan_options.lp_select_query = plan;
	/* Note: plan_options.table would have been set by caller if it is "emit_sql_statement" */
	/* Note: plan_options.function would have been set by caller if it is "emit_sql_statement" */
	if (FALSE == lp_verify_structure(plan, &plan_options)) {
		assert(FALSE);
		ERROR(ERR_PLAN_NOT_WELL_FORMED, "");
		return NULL;
	}
	out = allocate_physical_plan(plan, &plan_options, options);
	out->regexmatch_invoked = plan_options.regexmatch_invoked;
	if (LP_INSERT_INTO == plan->type) {
		/* A physical plan for the destination table of the INSERT INTO has been generated above.
		 * Now generate a separate physical plan for source table/query of the INSERT INTO.
		 */
		LogicalPlan *lp_insert_into_options, *lp_insert_into_more_options;
		GET_LP(lp_insert_into_options, plan, 1, LP_INSERT_INTO_OPTIONS);
		GET_LP(lp_insert_into_more_options, lp_insert_into_options, 1, LP_INSERT_INTO_MORE_OPTIONS);

		PhysicalPlan *src;
		src = generate_physical_plan(lp_insert_into_more_options->v.lp_default.operand[0], &plan_options);
		if (NULL == src) {
			return NULL;
		}
		return out;
	}
	if ((LP_SELECT_QUERY == plan->type) || (LP_TABLE_VALUE == plan->type)) {
		SqlTableAlias *root_table_alias;
		root_table_alias = plan->extra_detail.lp_select_query.root_table_alias;
		/* Note: root_table_alias can be NULL for xref plans (which do not correspond to any actual user-specified query) */
		out->aggregate_function_or_group_by_or_having_specified
		    = ((NULL == root_table_alias) ? FALSE : root_table_alias->aggregate_function_or_group_by_or_having_specified);

		LogicalPlan *output;
		// Set my output key
		GET_LP(output, plan, 1, LP_OUTPUT);

		LogicalPlan *output_key;
		GET_LP(output_key, output, 0, LP_KEY);
		out->outputKey = output_key->v.lp_key.key;
		out->is_cross_reference_key = out->outputKey->is_cross_reference_key;

		// If there is an ORDER BY, note it down
		if (NULL != output->v.lp_default.operand[1]) {
			assert(LP_TABLE_VALUE != plan->type);
			GET_LP(out->order_by, output, 1, LP_ORDER_BY);
		}
		out->projection = lp_get_projection_columns(plan);
	} else {
		out->aggregate_function_or_group_by_or_having_specified = FALSE;
	}
	if (LP_TABLE_VALUE != plan->type) {
		// See if there are any tables we rely on in the SELECT tablejoin list. If so, add them as prev records in physical
		// plan.
		select = lp_get_select(plan);
		GET_LP(table_joins, select, 0, LP_TABLE_JOIN);
		out->tablejoin = table_joins;
		do {
			assert(LP_TABLE_JOIN == table_joins->type);
			// If this is a plan that doesn't have a source table,
			//  this will be null and we need to skip this step
			if (NULL == table_joins->v.lp_default.operand[0]) {
				break;
			}
			type = table_joins->v.lp_default.operand[0]->type;
			if ((LP_SELECT_QUERY == type) || (LP_SET_OPERATION == type) || (LP_TABLE_VALUE == type)
			    || (LP_VIEW == type)) {
				PhysicalPlan	   *ret;
				PhysicalPlanOptions tmp_options;
				LogicalPlan	   *select_or_set_or_table_value_or_view;

				/* This is a fresh sub-query start so do not inherit any DNF context from parent query. */
				tmp_options = plan_options;
				tmp_options.dnf_plan_next = NULL;
				/* By the same token, do not inherit any "stash_columns_in_keys" context from parent query */
				tmp_options.stash_columns_in_keys = FALSE;
				// This is a sub plan, and should be inserted as prev
				GET_LP(select_or_set_or_table_value_or_view, table_joins, 0, type);
				ret = generate_physical_plan(select_or_set_or_table_value_or_view, &tmp_options);
				if (NULL == ret) {
					return NULL;
				}
			}
			GET_LP_ALLOW_NULL(table_joins, table_joins, 1, LP_TABLE_JOIN);
		} while (NULL != table_joins);
		// Iterate through the keys in the logical plan and use them to fill out the "iterKeys[]" array in physical plan
		keys = lp_get_keys(plan);
		assert((NULL != keys->v.lp_default.operand[0]));
		if (0 != iterate_keys(out, keys)) {
			return NULL;
		}
		// Note: The below do/while loop can be done only after we have initialized "iterKeys[]" for this table_join.
		//       That is why this is not done as part of the previous do/while loop as "iterate_keys()" gets called only
		//       after the previous do/while loop.
		table_joins = out->tablejoin;
		assert(NO_JOIN == table_joins->extra_detail.lp_table_join.cur_join_type);
		int num_left_joins, num_right_or_full_joins;
		num_left_joins = 0;
		num_right_or_full_joins = 0;
		do {
			LogicalPlan *join_on_condition;

			switch (table_joins->extra_detail.lp_table_join.cur_join_type) {
			case LEFT_JOIN:
				num_left_joins++;
				break;
			case RIGHT_JOIN:
			case FULL_JOIN:
				num_right_or_full_joins++;
				break;
			default:
				break;
			}
			/* See if there are any sub-queries in the ON clause of any JOINs. If so, generate separate physical plans
			 * (deferred and/or non-deferred) for them and add them as prev records in physical plan.
			 */
			join_on_condition = table_joins->extra_detail.lp_table_join.join_on_condition;
			if (NULL != join_on_condition) {
				sub_query_check_and_generate_physical_plan(&plan_options, join_on_condition, NULL);
			}
			GET_LP_ALLOW_NULL(table_joins, table_joins, 1, LP_TABLE_JOIN);
		} while (NULL != table_joins);
		out->key_lvn_can_be_zysqlnull = ((0 != num_left_joins) && (0 == num_right_or_full_joins));
		/* See if there are any sub-queries in the WHERE clause. If so, generate separate physical plans
		 * (deferred and/or non-deferred) for them and add them as prev records in physical plan.
		 */
		where = lp_get_select_where(plan);
		/* Note: If where->v.lp_default.operand[1] is non-NULL, this is the alternate list that
		 * "lp_optimize_where_multi_equals_ands_helper()" built that needs to be checked too for deferred plans which
		 * would have been missed out in case the keys for those had been fixed to keys from parent queries (see
		 * comment above "lp_get_select_where()" function call in "lp_optimize_where_multi_equals_ands_helper()").
		 * The below call takes care of that too.
		 */
		out->in_where_clause = TRUE;
		out->where = sub_query_check_and_generate_physical_plan(&plan_options, where, NULL);
		out->in_where_clause = FALSE;
		out->keywords = lp_get_select_keywords(plan)->v.lp_keywords.keywords;
		switch (plan->type) {
		case LP_DELETE_FROM:
			break;
		case LP_UPDATE:; /* semicolon for empty statement so we can declare variables in case block */
			LogicalPlan *lp_column_list;

			lp_column_list = lp_get_update_column_list(plan);
			sub_query_check_and_generate_physical_plan(&plan_options, lp_column_list, NULL);
			break;
		default:
			assert(LP_SELECT_QUERY == plan->type);
			/* See if there are any sub-queries in the SELECT column list. If so, generate separate physical plans
			 * (deferred and/or non-deferred) for them and add them as prev records in physical plan.
			 */
			project = lp_get_project(plan);
			GET_LP(select_column_list, project, 0, LP_COLUMN_LIST);
			sub_query_check_and_generate_physical_plan(&plan_options, select_column_list, NULL);
			// Check GROUP BY and HAVING
			GET_LP(criteria, select, 1, LP_CRITERIA);
			GET_LP(select_options, criteria, 1, LP_SELECT_OPTIONS);
			GET_LP(select_more_options, select_options, 1, LP_SELECT_MORE_OPTIONS);
			if (NULL != select_more_options->v.lp_default.operand[0]) {
				GET_LP(out->aggregate_options, select_more_options, 0, LP_AGGREGATE_OPTIONS);
			}
			/* See if there are any sub-queries in the HAVING clause. If so, generate separate physical plans
			 * (deferred and/or non-deferred) for them and add them as prev records in physical plan.
			 */
			if (NULL != out->aggregate_options) {
				int	     i;
				LogicalPlan *group_by_or_having;

				/* Note: LP_GROUP_BY is lp_default_operand[0], LP_HAVING is lp_default_operand[1] */
				for (i = 0; i < 2; i++) {
					group_by_or_having = out->aggregate_options->v.lp_default.operand[i];
					/* cannot use GET_LP as LP_GROUP_BY and/or LP_HAVING can be NULL */
					if (NULL != group_by_or_having) {
						out->aggregate_options->v.lp_default.operand[i]
						    = sub_query_check_and_generate_physical_plan(&plan_options, group_by_or_having,
												 NULL);
					}
				}
			}
			/* See if there are any sub-queries in the ORDER BY clause. If so, generate separate physical plans
			 * (deferred and/or non-deferred) for them and add them as prev records in physical plan.
			 */
			if (NULL != out->order_by) {
				sub_query_check_and_generate_physical_plan(&plan_options, out->order_by, NULL);
			}
			// Check for DISTINCT keyword (possible only for LP_SELECT_QUERY plan)
			keyword = get_keyword_from_keywords(out->keywords, OPTIONAL_DISTINCT);
			if (NULL != keyword) {
				out->distinct_values = TRUE;
				out->maintain_columnwise_index = TRUE;
			}
			break;
		}
		// Check for OPTIONAL_BOOLEAN_EXPANSION keyword (possible for LP_SELECT_QUERY/LP_DELETE_FROM/LP_UPDATE plans)
		keyword = get_keyword_from_keywords(out->keywords, OPTIONAL_BOOLEAN_EXPANSION);
		if (NULL != keyword) {
			/* Note that "emit_duplication_check" is necessary for LP_SELECT_QUERY and LP_UPDATE but not for
			 * LP_DELETE_FROM. It is kept to true since it keeps the code for LP_UPDATE and LP_DELETE_FROM consistent.
			 * See #579 commit message for details on why LP_UPDATE needs it and LP_DELETE_FROM does not.
			 */
			out->emit_duplication_check = TRUE;
			/* "maintain_columnwise_index" makes sense only for a LP_SELECT_QUERY. */
			if (LP_SELECT_QUERY == plan->type) {
				out->maintain_columnwise_index = TRUE; /* needs to be set in all DNF siblings */
			}
			if (NULL != plan_options.dnf_plan_next) {
				/* Caller indicates this plan is part of a set of DNF plans. Maintain linked list of DNF siblings.
				 */
				out->dnf_next = plan_options.dnf_plan_next;
				out->dnf_num = out->dnf_next->dnf_num; /* Maintain unique DNF sibling plan number */
				assert(NULL == plan_options.dnf_plan_next->dnf_prev);
				plan_options.dnf_plan_next->dnf_prev = out;
				assert(NULL == out->dnf_prev);
			}
			out->dnf_num++;
		}
	} else {
		/* VALUES clause. Check for any sub-queries in the data specified across multiple rows/columns */
		LogicalPlan *lp_table_data, *lp_row_value;

		GET_LP(lp_table_data, plan, 0, LP_TABLE_DATA);
		GET_LP(lp_row_value, lp_table_data, 1, LP_ROW_VALUE);
		do {
			LogicalPlan *lp_column_list;

			GET_LP(lp_column_list, lp_row_value, 0, LP_COLUMN_LIST);
			sub_query_check_and_generate_physical_plan(&plan_options, lp_column_list, NULL);
			GET_LP_ALLOW_NULL(lp_row_value, lp_row_value, 1, LP_ROW_VALUE);
		} while (NULL != lp_row_value);
	}
	out->stash_columns_in_keys = options->stash_columns_in_keys;

	/* Note down the physical plan "out" and all its dependent plans (going back using "->prev" in linked list up to
	 * "first_pplan") as it will be used to avoid duplicate physical plan processing. We note down the end of the linked
	 * list (in the "prev" direction) by setting "dependent_plans_end" field to that.
	 */
	PhysicalPlan *first_pplan;
	first_pplan = *plan_options.last_plan;
	assert(NULL != first_pplan);
	assert(NULL == first_pplan->prev);
	out->dependent_plans_end = first_pplan;

	/* Check if YDBOcto#617 optimization can be applied on this physical plan */
	out->is_octo617_optimized = is_octo617_optimization_possible(out);
	if (out->is_octo617_optimized) {
		/* Now that we know optimization is possible, check to see if there are any non-key columns where
		 * aggregate functions were used. If so, generate xref plans for those.
		 */
		LogicalPlan *af, *first_aggregate;
		first_aggregate = out->lp_select_query->extra_detail.lp_select_query.first_aggregate;
		for (af = first_aggregate; NULL != af; af = af->extra_detail.lp_aggregate_function.next_aggregate) {
			LogicalPlan *stmt;
			stmt = af->extra_detail.lp_aggregate_function.octo617_xref_plan;
			if (NULL != stmt) {
				generate_physical_plan(stmt, &plan_options);
			}
		}
	}
	return out;
}

int iterate_keys(PhysicalPlan *out, LogicalPlan *plan) {
	LogicalPlan *left, *right;

	assert(LP_KEYS == plan->type);
	GET_LP(left, plan, 0, LP_KEY);
	if (MAX_KEY_COUNT > out->total_iter_keys) {
		out->iterKeys[out->total_iter_keys] = left->v.lp_key.key;
	}
	out->total_iter_keys++;
	if (NULL != plan->v.lp_default.operand[1]) {
		GET_LP(right, plan, 1, LP_KEYS);
		return iterate_keys(out, right);
	} else if (MAX_KEY_COUNT <= out->total_iter_keys) {
		ERROR(ERR_TOO_MANY_SELECT_KEYCOLS, out->total_iter_keys, MAX_KEY_COUNT);
		return 1;
	}
	return 0;
}

LogicalPlan *sub_query_check_and_generate_physical_plan(PhysicalPlanOptions *options, LogicalPlan *stmt, LogicalPlan *parent) {
	int i;

	if (NULL == stmt) {
		return NULL;
	}
	assert(LP_UNARY_LAST != stmt->type);
	if ((LP_ADDITION <= stmt->type) && (LP_BOOLEAN_LAST > stmt->type)) {
		stmt->v.lp_default.operand[0]
		    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
		stmt->v.lp_default.operand[1]
		    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[1], stmt);
	} else if ((LP_FORCE_NUM <= stmt->type) && (LP_UNARY_LAST > stmt->type)) {
		stmt->v.lp_default.operand[0]
		    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
	} else {
		PhysicalPlanOptions plan_options;
		PhysicalPlan	   *cur, *child_plan;
		SqlTableAlias	   *table_alias;
		SqlColumnAlias	   *column_alias;
		PhysicalPlan	   *new_plan;
		LogicalPlan	   *plan, *oper1;
		int		    unique_id;
		boolean_t	    set_deferred_plan;

		switch (stmt->type) {
		case LP_KEY:
			/* No action */
			break;
		case LP_WHERE:
		case LP_GROUP_BY:
		case LP_HAVING:
			stmt->v.lp_default.operand[0]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
			if (LP_WHERE != stmt->type) {
				assert((LP_GROUP_BY == stmt->type) || (LP_HAVING == stmt->type));
				break;
			}
			oper1 = stmt->v.lp_default.operand[1];
			if ((NULL != oper1) && (LP_COLUMN_LIST_ALIAS != oper1->type)) {
				/* Note that it is possible we have `operand[1]` set to a non-NULL value for a LP_WHERE.
				 * In this case, this is the alternate list that "lp_optimize_where_multi_equals_ands_helper()"
				 * built that needs to also be checked for sub-queries.
				 */
				stmt->v.lp_default.operand[1]
				    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[1], stmt);
			}
			break;
		case LP_DERIVED_COLUMN:
		case LP_COLUMN_ALIAS:;
			/* Check if this value is a parent reference; if so, mark this physical plan as deferred.
			 * Note that this physical plan might already be marked as deferred from a previous parent query
			 * column reference. In that case, compare the two to see which physical plan is the closest ancestor
			 * to the current physical plan and set that to be the deferred plan for this physical plan.
			 */
			boolean_t *in_where_clause;
			if (LP_COLUMN_ALIAS == stmt->type) {
				column_alias = stmt->v.lp_column_alias.column_alias;
				UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
				unique_id = table_alias->unique_id;
				in_where_clause = &stmt->v.lp_column_alias.in_where_clause;
			} else {
				LogicalPlan *output_key;

				assert(LP_DERIVED_COLUMN == stmt->type);
				GET_LP(output_key, stmt, 0, LP_KEY);
				unique_id = output_key->v.lp_key.key->unique_id;
				in_where_clause = &stmt->extra_detail.lp_derived_column.in_where_clause;
			}
			cur = options->parent;
			child_plan = cur;
			set_deferred_plan = FALSE;
			while (NULL != cur) {
				unsigned int iter_key_index;

				for (iter_key_index = 0; iter_key_index < cur->total_iter_keys; iter_key_index++) {
					if (cur->iterKeys[iter_key_index]->unique_id == unique_id) {
						/* Note down if WHERE clause is being processed */
						*in_where_clause = cur->in_where_clause;
						break;
					}
				}
				if (iter_key_index != cur->total_iter_keys) {
					break;
				}
				cur = cur->parent_plan;
				set_deferred_plan = TRUE;
			}
			if (NULL == cur) {
				assert(FALSE);
				ERROR(ERR_PLAN_OWNER, "");
			}
			if (set_deferred_plan) {
				do {
					child_plan->is_deferred_plan = TRUE;
					child_plan = child_plan->parent_plan;
				} while (child_plan != cur);
			}
			/* No action */
			break;
		case LP_VALUE:
			/* No action */
			break;
		case LP_SELECT_QUERY:
		case LP_SET_OPERATION:
		case LP_TABLE_VALUE:
			/* Generate a separate physical plan for this sub-query.
			 * This is a fresh sub-query start so do not inherit any DNF context from parent query.
			 */
			plan_options = *options;
			plan_options.dnf_plan_next = NULL;
			if ((NULL != parent)
			    && ((LP_BOOLEAN_EXISTS == parent->type) || (LP_BOOLEAN_NOT_EXISTS == parent->type)
				|| (LP_ARRAY == parent->type))) {
				/* If sub-query is due to an EXISTS or NOT EXISTS operation, then do not stash columns in keys.
				 * Keep it as is (it can have any # of columns).
				 * If sub-query is due to an ARRAY operation, then again do not stash columns in keys.
				 * Keep it as is as otherwise GetScalarOrArray^%ydboctoplanhelpers might get confused
				 * and/or return incorrect results.
				 */
				plan_options.stash_columns_in_keys = FALSE;
			} else {
				/* Otherwise, we are guaranteed only 1 column in sub-query so stash it in a key (due to the
				 * check done in "src/populate_data_type.c" (search for
				 * ISSUE_ERR_AND_BREAK_IF_SUBQUERY_HAS_MULTIPLE_COLUMNS usage).
				 */
				assert(1 == lp_get_num_cols_in_select_column_list(stmt));
				plan_options.stash_columns_in_keys = TRUE;
			}
			new_plan = generate_physical_plan(stmt, &plan_options);
			if (NULL == new_plan) {
				return NULL;
			}
			break;
		case LP_UPD_COL_VALUE:
			assert(LP_COLUMN == stmt->v.lp_default.operand[0]->type); /* subqueries are not possible here */
			stmt->v.lp_default.operand[1]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[1], stmt);
			break;
		case LP_ARRAY:
			assert((LP_SELECT_QUERY == stmt->v.lp_default.operand[0]->type)
			       || (LP_SET_OPERATION == stmt->v.lp_default.operand[0]->type)
			       || (LP_TABLE_VALUE == stmt->v.lp_default.operand[0]->type));
			stmt->v.lp_default.operand[0]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
			break;
		case LP_COALESCE_CALL:
		case LP_GREATEST:
		case LP_LEAST:
			assert(LP_COLUMN_LIST == stmt->v.lp_default.operand[0]->type);
			stmt->v.lp_default.operand[0]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
			break;
		case LP_NULL_IF:
			stmt->v.lp_default.operand[0]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
			stmt->v.lp_default.operand[1]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[1], stmt);
			break;
		case LP_FUNCTION_CALL:
			assert(LP_VALUE == stmt->v.lp_default.operand[0]->type);
			stmt->v.lp_default.operand[0]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
			assert(LP_COLUMN_LIST == stmt->v.lp_default.operand[1]->type);
			stmt->v.lp_default.operand[1]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[1], stmt);
			break;
		case LP_AGGREGATE_FUNCTION_COUNT:
		case LP_AGGREGATE_FUNCTION_COUNT_ASTERISK:
		case LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK:
		case LP_AGGREGATE_FUNCTION_AVG:
		case LP_AGGREGATE_FUNCTION_MIN:
		case LP_AGGREGATE_FUNCTION_MAX:
		case LP_AGGREGATE_FUNCTION_SUM:
		case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT:
		case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK:
		case LP_AGGREGATE_FUNCTION_AVG_DISTINCT:
		case LP_AGGREGATE_FUNCTION_SUM_DISTINCT:
		case LP_CASE:
		case LP_CASE_STATEMENT:
		case LP_CASE_BRANCH:
		case LP_CASE_BRANCH_STATEMENT:
			for (i = 0; i < 2; i++) {
				stmt->v.lp_default.operand[i]
				    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[i], stmt);
			}
			break;
		case LP_COLUMN_LIST:
			/* To avoid a large recursion stack in case of thousands of columns, walk the column list iteratively */
			plan = stmt;
			while (NULL != plan) {
				assert(LP_COLUMN_LIST != plan->v.lp_default.operand[0]->type);
				sub_query_check_and_generate_physical_plan(options, plan->v.lp_default.operand[0], plan);
				GET_LP_ALLOW_NULL(plan, plan, 1, LP_COLUMN_LIST);
			}
			break;
		case LP_ORDER_BY:
			/* In the ORDER BY COLUMN NUM case, the ORDER BY plan (LP_ORDER_BY -> LP_COLUMN_LIST -> LP_WHERE)
			 * points to the same plan as the SELECT column list (LP_PROJECT -> LP_COLUMN_LIST -> ... -> LP_WHERE)
			 * and so do not descend down the LP_COLUMN_LIST plan (operand[0]) to avoid duplicate descents as this
			 * will cause multiple `unique_id` to be created for the same sub-query in case sub-queries are present
			 * in the SELECT column list (one for the SELECT column list reference and one for the ORDER BY COLUMN NUM
			 * reference) which is incorrect. For operand[1], the same logic applies if it is non-NULL and is an
			 * ORDER BY COLUMN NUM reference too but that would be caught by the recursive call below.
			 */
			if (!stmt->extra_detail.lp_order_by.order_by_column_num) {
				stmt->v.lp_default.operand[0]
				    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[0], stmt);
			}
			stmt->v.lp_default.operand[1]
			    = sub_query_check_and_generate_physical_plan(options, stmt->v.lp_default.operand[1], stmt);
			break;
		case LP_KEYWORDS:
			// This is an UPDATE statement with a DEFAULT SET value for an IDENTITY column
			assert(OPTIONAL_DEFAULT == stmt->v.lp_keywords.keywords->keyword);
			break;
		case LP_TABLE:
			// This should never happen; fall through to error case
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			return NULL;
			break;
		}
	}
	return stmt;
}
