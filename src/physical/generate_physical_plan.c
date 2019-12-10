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
#include "physical_plan.h"

#include "template_helpers.h"

void gen_source_keys(PhysicalPlan *out, LogicalPlan *plan);
void iterate_keys(PhysicalPlan *out, LogicalPlan *plan);
LogicalPlan *walk_where_statement(PhysicalPlanOptions *options, LogicalPlan *stmt);

PhysicalPlan *generate_physical_plan(LogicalPlan *plan, PhysicalPlanOptions *options) {
	SqlOptionalKeyword	*keywords, *keyword;
	LogicalPlan		*keys, *table_joins, *select, *insert_or_set_operation, *output_key, *output;
	LogicalPlan		*criteria, *select_options, *select_more_options;
	LogicalPlan		*where;
	LogicalPlan		*set_option, *set_plans, *set_output, *set_key;
	PhysicalPlan		*out, *prev = NULL;
	LPActionType		set_oper_type, type;
	PhysicalPlanOptions	plan_options;
	boolean_t		is_set_dnf;
	SqlTableAlias		*root_table_alias;

	plan_options = *options;
	assert((LP_INSERT == plan->type) || (LP_SET_OPERATION == plan->type));
	// If this is a union plan, construct physical plans for the two children
	if (LP_SET_OPERATION == plan->type) {
		GET_LP(set_option, plan, 0, LP_SET_OPTION);
		GET_LP(set_plans, plan, 1, LP_PLANS);
		out = generate_physical_plan(set_plans->v.lp_default.operand[1], &plan_options);
		if (NULL == out) {
			return NULL;
		}
		assert(NULL != out->lp_insert);
		set_oper_type = set_option->v.lp_default.operand[0]->type;
		assert((LP_SET_UNION == set_oper_type) || (LP_SET_UNION_ALL == set_oper_type) || (LP_SET_DNF == set_oper_type)
			|| (LP_SET_EXCEPT == set_oper_type) || (LP_SET_EXCEPT_ALL == set_oper_type)
			|| (LP_SET_INTERSECT == set_oper_type) || (LP_SET_INTERSECT_ALL == set_oper_type));
		is_set_dnf = (LP_SET_DNF == set_oper_type);
		if (is_set_dnf) {
			PhysicalPlan	*next, *tmp;

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
		prev = generate_physical_plan(set_plans->v.lp_default.operand[0], &plan_options);
		if (NULL == prev) {
			return NULL;
		}
		assert(NULL != prev->lp_insert);
		if (!is_set_dnf) {
			int		input_id1, input_id2;
			SetOperType	*set_oper, *prev_oper, *out_oper, *next_oper;

			OCTO_CMALLOC_STRUCT(set_oper, SetOperType);
			prev_oper = prev->set_oper_list;
			input_id1 = (NULL == prev_oper) ? prev->outputKey->unique_id : prev_oper->output_id;
			assert(input_id1 != out->outputKey->unique_id);
			out_oper = out->set_oper_list;
			input_id2 = (NULL == out_oper) ? out->outputKey->unique_id : out_oper->output_id;
			set_oper->set_oper_type = set_oper_type;
			set_oper->input_id1 = input_id1;
			set_oper->input_id2 = input_id2;
			GET_LP(set_output, set_option, 1, LP_OUTPUT);
			GET_LP(set_key, set_output, 0, LP_KEY);
			set_oper->output_id = set_key->v.lp_key.key->unique_id;
			set_oper->prev = NULL;
			next_oper = out->set_oper_list;
			set_oper->next = next_oper;
			if (NULL != next_oper)
				next_oper->prev = set_oper;
			out->set_oper_list = set_oper;
		}
		return out;
	}
	// Make sure the plan is in good shape. Overload this plan verify phase to also fix aggregate function counts.
	assert(LP_INSERT == plan->type);
	if (FALSE == lp_verify_structure(plan, &plan->extra_detail.lp_insert.first_aggregate)) {
		/// TODO: replace this with a real error message
		ERROR(ERR_PLAN_NOT_WELL_FORMED, "");
		return NULL;
	}
	OCTO_CMALLOC_STRUCT(out, PhysicalPlan);
	out->parent_plan = options->parent;
	out->lp_insert = plan;
	root_table_alias = plan->extra_detail.lp_insert.root_table_alias;
	/* Note: root_table_alias can be NULL for xref plans (which do not correspond to any actual user-specified query) */
	out->aggregate_function_or_group_by_specified = ((NULL == root_table_alias)
								? FALSE
								: root_table_alias->aggregate_function_or_group_by_specified);
	plan_options.parent = out;
	out->next = *plan_options.last_plan;
	if (NULL != *plan_options.last_plan) {
		(*plan_options.last_plan)->prev = out;
	}
	*(plan_options.last_plan) = out;

	// Set my output key
	GET_LP(output, plan, 1, LP_OUTPUT);
	if (LP_KEY == output->v.lp_default.operand[0]->type) {
		GET_LP(output_key, output, 0, LP_KEY);
		out->outputKey = output_key->v.lp_key.key;
		out->is_cross_reference_key = out->outputKey->is_cross_reference_key;
	} else {
		assert(FALSE);
	}

	// If there is an ORDER BY, note it down
	if (output->v.lp_default.operand[1]) {
		GET_LP(out->order_by, output, 1, LP_ORDER_BY);
	}

	// See if there are any tables we rely on in the SELECT tablejoin list. If so, add them as prev records in physical plan.
	select = lp_get_select(plan);
	GET_LP(table_joins, select, 0, LP_TABLE_JOIN);
	out->tablejoin = table_joins;
	do {
		assert(LP_TABLE_JOIN == table_joins->type);
		// If this is a plan that doesn't have a source table,
		//  this will be null and we need to skip this step
		if (NULL == table_joins->v.lp_default.operand[0])
			break;
		type = table_joins->v.lp_default.operand[0]->type;
		if ((LP_INSERT == type) || (LP_SET_OPERATION == type)) {
			PhysicalPlan	*ret;

			// This is a sub plan, and should be inserted as prev
			GET_LP(insert_or_set_operation, table_joins, 0, type);
			ret = generate_physical_plan(insert_or_set_operation, &plan_options);
			if (NULL == ret)
				return NULL;
		}
		table_joins = table_joins->v.lp_default.operand[1];
	} while (NULL != table_joins);
	// Check GROUP BY and HAVING
	GET_LP(criteria, select, 1, LP_CRITERIA);
	GET_LP(select_options, criteria, 1, LP_SELECT_OPTIONS);
	GET_LP(select_more_options, select_options, 1, LP_SELECT_MORE_OPTIONS);
	if (NULL != select_more_options->v.lp_default.operand[0]) {
		GET_LP(out->aggregate_options, select_more_options, 0, LP_AGGREGATE_OPTIONS);
	}
	// Iterate through the key substructures and fill out the source keys
	keys = lp_get_keys(plan);
	// All tables should have at least one key
	assert(NULL != keys);
	// Either we have some keys already, or we have a list of keys
	assert((0 < out->total_iter_keys) || (NULL != keys->v.lp_default.operand[0]));
	if (keys->v.lp_default.operand[0])
		iterate_keys(out, keys);
	// Note: The below do/while loop can be done only after we have initialized "iterKeys[]" for this table_join.
	//       That is why this is not done as part of the previous do/while loop as "iterate_keys()" gets called only after
	//       the previous do/while loop.
	table_joins = out->tablejoin;
	do {
		LogicalPlan	*join_on_condition;

		// See if there are any tables we rely on in the ON clause of any JOINs.
		// If so, add them as prev records in physical plan.
		join_on_condition = table_joins->extra_detail.lp_table_join.join_on_condition;
		if (NULL != join_on_condition) {
			walk_where_statement(&plan_options, join_on_condition);
		}
		table_joins = table_joins->v.lp_default.operand[1];
	} while (NULL != table_joins);
	// See if there are any tables we rely on in the WHERE clause. If so, add them as prev records in physical plan.
	where = lp_get_select_where(plan);
	out->where = walk_where_statement(&plan_options, where);
	if (NULL != where->v.lp_default.operand[1])
	{	/* If where->v.lp_default.operand[1] is non-NULL, this is the alternate list that
		 * "lp_optimize_where_multi_equal_ands_helper()" built that needs to be checked too for deferred plans which
		 * would have been missed out in case the keys for those had been fixed to keys from parent queries (see
		 * comment above "lp_get_select_where()" function call in "lp_optimize_where_multi_equal_ands_helper()").
		 */
		walk_where_statement(&plan_options, where->v.lp_default.operand[1]);
		where->v.lp_default.operand[1] = NULL;	/* Discard alternate list now that its purpose is served */
	}
	// See if there are any tables we rely on in the SELECT column list. If so, add them as prev records in physical plan.
	walk_where_statement(&plan_options, lp_get_project(plan)->v.lp_default.operand[0]->v.lp_default.operand[0]);
	out->keywords = lp_get_select_keywords(plan)->v.lp_keywords.keywords;
	out->projection = lp_get_projection_columns(plan);
	// See if there are any tables we rely on in the HAVING clause. If so, add them as prev records in physical plan.
	if (NULL != out->aggregate_options) {
		LogicalPlan	*having;

		having = out->aggregate_options->v.lp_default.operand[1];
		if (NULL != having) {
			out->aggregate_options->v.lp_default.operand[1] = walk_where_statement(&plan_options, having);
		}
	}
	// Check the optional words for distinct
	keywords = lp_get_select_keywords(plan)->v.lp_keywords.keywords;
	keyword = get_keyword_from_keywords(keywords, OPTIONAL_DISTINCT);
	if (NULL != keyword) {
		out->distinct_values = TRUE;
		out->maintain_columnwise_index = TRUE;
	}
	keyword = get_keyword_from_keywords(keywords, OPTIONAL_BOOLEAN_EXPANSION);
	if (NULL != keyword) {
		out->emit_duplication_check = TRUE;
		out->maintain_columnwise_index = TRUE;	/* needs to be set in all DNF siblings */
		if (NULL != plan_options.dnf_plan_next) {
			/* Caller indicates this plan is part of a set of DNF plans. Maintain liked list of DNF siblings. */
			out->dnf_next = plan_options.dnf_plan_next;
			assert(NULL == plan_options.dnf_plan_next->dnf_prev);
			plan_options.dnf_plan_next->dnf_prev = out;
			assert(NULL == out->dnf_prev);
		}
	}
	out->stash_columns_in_keys = options->stash_columns_in_keys;
	return out;
}

void iterate_keys(PhysicalPlan *out, LogicalPlan *plan) {
	LogicalPlan	*left, *right;

	assert(LP_KEYS == plan->type);
	GET_LP(left, plan, 0, LP_KEY);
	out->iterKeys[out->total_iter_keys] = left->v.lp_key.key;
	out->total_iter_keys++;
	if (NULL != plan->v.lp_default.operand[1]) {
		GET_LP(right, plan, 1, LP_KEYS);
		iterate_keys(out, right);
	}
}

LogicalPlan *walk_where_statement(PhysicalPlanOptions *options, LogicalPlan *stmt) {

	if (NULL == stmt)
		return NULL;
	assert(LP_UNARY_LAST != stmt->type);
	if ((LP_ADDITION <= stmt->type) && (LP_BOOLEAN_LAST > stmt->type)) {
		stmt->v.lp_default.operand[0] = walk_where_statement(options, stmt->v.lp_default.operand[0]);
		stmt->v.lp_default.operand[1] = walk_where_statement(options, stmt->v.lp_default.operand[1]);
	} else if ((LP_FORCE_NUM <= stmt->type) && (LP_UNARY_LAST > stmt->type)) {
		stmt->v.lp_default.operand[0] = walk_where_statement(options, stmt->v.lp_default.operand[0]);
	} else {
		PhysicalPlanOptions	plan_options;
		PhysicalPlan		*cur, *out;
		SqlTableAlias		*table_alias;
		PhysicalPlan		*new_plan;

		switch(stmt->type) {
		case LP_KEY:
			/* No action */
			break;
		case LP_DERIVED_COLUMN:
			/* No action */
			break;
		case LP_WHERE:
		case LP_HAVING:
			stmt->v.lp_default.operand[0] = walk_where_statement(options, stmt->v.lp_default.operand[0]);
			break;
		case LP_COLUMN_ALIAS:
			// Check if this value is a parent reference; if so, mark this as deferred
			out = options->parent;
			cur = out;
			UNPACK_SQL_STATEMENT(table_alias, stmt->v.lp_column_alias.column_alias->table_alias, table_alias);
			while (cur) {
				unsigned int	i;

				for (i = 0; i < out->total_iter_keys; i++)
					if (cur->iterKeys[i]->unique_id == table_alias->unique_id)
						break;
				if (i != cur->total_iter_keys)
					break;
				cur->deferred_plan = TRUE;
				cur = cur->parent_plan;
			}
			if (NULL == cur) {
				WARNING(CUSTOM_ERROR, "Problem resolving owner for deferred plan; undefined behavior");
			}
			/* No action */
			break;
		case LP_VALUE:
			/* No action */
			break;
		case LP_INSERT:
		case LP_SET_OPERATION:
			// Generate a separate physical plan for this sub-query.
			plan_options = *options;
			plan_options.stash_columns_in_keys = TRUE;
			new_plan = generate_physical_plan(stmt, &plan_options);
			if (NULL == new_plan)
				return NULL;
			break;
		case LP_FUNCTION_CALL:
		case LP_AGGREGATE_FUNCTION_COUNT_ASTERISK:
		case LP_AGGREGATE_FUNCTION_COUNT:
		case LP_AGGREGATE_FUNCTION_AVG:
		case LP_AGGREGATE_FUNCTION_MIN:
		case LP_AGGREGATE_FUNCTION_MAX:
		case LP_AGGREGATE_FUNCTION_SUM:
		case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT:
		case LP_AGGREGATE_FUNCTION_AVG_DISTINCT:
		case LP_AGGREGATE_FUNCTION_SUM_DISTINCT:
		case LP_COLUMN_LIST:
		case LP_CASE:
		case LP_CASE_STATEMENT:
		case LP_CASE_BRANCH:
		case LP_CASE_BRANCH_STATEMENT:
			stmt->v.lp_default.operand[0] = walk_where_statement(options, stmt->v.lp_default.operand[0]);
			stmt->v.lp_default.operand[1] = walk_where_statement(options, stmt->v.lp_default.operand[1]);
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
