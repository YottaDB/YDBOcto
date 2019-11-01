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
LogicalPlan *walk_where_statement(PhysicalPlanOptions *options, LogicalPlan *stmt, LogicalPlan *parent);

#define	SET_PLAN_PROPERTY_IN_ALL_DNF_SIBLINGS(STARTPLAN, FIELDNAME, VALUE)	\
{										\
	PhysicalPlan	*cur;							\
										\
	cur = STARTPLAN;							\
	for ( ; ; )								\
	{									\
		cur->FIELDNAME = VALUE;						\
		cur = cur->next;						\
		if (NULL == cur)						\
			break;							\
		if (!is_prev_plan_in_same_dnf(cur))				\
			break;							\
	}									\
}

PhysicalPlan *generate_physical_plan(LogicalPlan *plan, PhysicalPlanOptions *options) {
	SqlOptionalKeyword	*keywords, *keyword;
	LogicalPlan		*keys, *table_joins, *select, *insert_or_set_operation, *output_key, *output;
	LogicalPlan		*where;
	LogicalPlan		*set_option, *set_plans, *set_output, *set_key;
	PhysicalPlan		*out, *prev = NULL;
	LPActionType		set_oper_type, type;
	PhysicalPlanOptions	curr_plan;

	curr_plan = *options;
	assert((LP_INSERT == plan->type) || (LP_SET_OPERATION == plan->type));
	// If this is a union plan, construct physical plans for the two children
	if (LP_SET_OPERATION == plan->type) {
		GET_LP(set_option, plan, 0, LP_SET_OPTION);
		GET_LP(set_plans, plan, 1, LP_PLANS);
		out = generate_physical_plan(set_plans->v.operand[1], &curr_plan);
		if (NULL == out)
			return NULL;
		prev = generate_physical_plan(set_plans->v.operand[0], &curr_plan);
		if (NULL == prev)
			return NULL;

		set_oper_type = set_option->v.operand[0]->type;
		assert((LP_SET_UNION == set_oper_type) || (LP_SET_UNION_ALL == set_oper_type) || (LP_SET_DNF == set_oper_type)
			|| (LP_SET_EXCEPT == set_oper_type) || (LP_SET_EXCEPT_ALL == set_oper_type)
			|| (LP_SET_INTERSECT == set_oper_type) || (LP_SET_INTERSECT_ALL == set_oper_type));
		if (LP_SET_DNF == set_oper_type)
		{
			SET_PLAN_PROPERTY_IN_ALL_DNF_SIBLINGS(out, maintain_columnwise_index, TRUE);
			SET_PLAN_PROPERTY_IN_ALL_DNF_SIBLINGS(prev, maintain_columnwise_index, TRUE);
		} else
		{
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
			set_oper->output_id = set_key->v.key->unique_id;
			set_oper->prev = NULL;
			next_oper = out->set_oper_list;
			set_oper->next = next_oper;
			if (NULL != next_oper)
				next_oper->prev = set_oper;
			out->set_oper_list = set_oper;
		}
		return out;
	}

	// Make sure the plan is in good shape
	if (FALSE == lp_verify_structure(plan)) {
		/// TODO: replace this with a real error message
		ERROR(ERR_PLAN_NOT_WELL_FORMED, "");
		return NULL;
	}
	OCTO_CMALLOC_STRUCT(out, PhysicalPlan);
	out->parent_plan = options->parent;
	curr_plan.parent = out;

	out->next = *curr_plan.last_plan;
	if (NULL != *curr_plan.last_plan) {
		(*curr_plan.last_plan)->prev = out;
	}
	*(curr_plan.last_plan) = out;

	// Set my output key
	GET_LP(output, plan, 1, LP_OUTPUT);
	if (LP_KEY == output->v.operand[0]->type) {
		GET_LP(output_key, output, 0, LP_KEY);
		out->outputKey = output_key->v.key;
		out->is_cross_reference_key = out->outputKey->is_cross_reference_key;
	} else if (LP_TABLE == output->v.operand[0]->type) {
		out->outputKey = NULL;
		out->outputTable = output->v.operand[1]->v.table_alias;
	} else {
		assert(FALSE);
	}

	// If there is an order by, note it down
	if (output->v.operand[1]) {
		GET_LP(out->order_by, output, 1, LP_COLUMN_LIST);
	}

	// See if there are any tables we rely on in the SELECT tablejoin list. If so, add them as prev records in physical plan.
	select = lp_get_select(plan);
	GET_LP(table_joins, select, 0, LP_TABLE_JOIN);
	out->tablejoin = table_joins;
	do {
		assert(LP_TABLE_JOIN == table_joins->type);
		// If this is a plan that doesn't have a source table,
		//  this will be null and we need to skip this step
		if (NULL == table_joins->v.operand[0])
			break;
		type = table_joins->v.operand[0]->type;
		if ((LP_INSERT == type) || (LP_SET_OPERATION == type)) {
			PhysicalPlan	*ret;

			// This is a sub plan, and should be inserted as prev
			GET_LP(insert_or_set_operation, table_joins, 0, type);
			ret = generate_physical_plan(insert_or_set_operation, &curr_plan);
			if (NULL == ret)
				return NULL;
		}
		table_joins = table_joins->v.operand[1];
	} while (NULL != table_joins);

	// Iterate through the key substructures and fill out the source keys
	keys = lp_get_keys(plan);
	// All tables should have at least one key
	assert(NULL != keys);
	// Either we have some keys already, or we have a list of keys
	assert((0 < out->total_iter_keys) || (NULL != keys->v.operand[0]));
	if (keys->v.operand[0])
		iterate_keys(out, keys);
	// Note: The below do/while loop can be done only after we have initialized "iterKeys[]" for this table_join.
	//       That is why this is not done as part of the previous do/while loop as "iterate_keys()" gets called only after
	//       the previous do/while loop.
	table_joins = out->tablejoin;
	do {
		// See if there are any tables we rely on in the ON clause of any JOINs.
		// If so, add them as prev records in physical plan.
		if (NULL != table_joins->join_on_condition) {
			walk_where_statement(&curr_plan, table_joins->join_on_condition, NULL);
		}
		table_joins = table_joins->v.operand[1];
	} while (NULL != table_joins);
	// See if there are any tables we rely on in the WHERE clause. If so, add them as prev records in physical plan.
	where = lp_get_select_where(plan);
	out->where = walk_where_statement(&curr_plan, where, NULL);
	if (NULL != where->v.operand[1])
	{	/* If where->v.operand[1] is non-NULL, this is the alternate list that "lp_optimize_where_multi_equal_ands_helper()"
		 * built that needs to be checked too for deferred plans which would have
		 * been missed out in case the keys for those had been fixed to keys from parent queries (see comment above
		 * "lp_get_select_where()" function call in "lp_optimize_where_multi_equal_ands_helper()").
		 */
		walk_where_statement(&curr_plan, where->v.operand[1], NULL);
		where->v.operand[1] = NULL;	/* Discard alternate list now that its purpose is served */
	}
	// See if there are any tables we rely on in the SELECT column list. If so, add them as prev records in physical plan.
	walk_where_statement(&curr_plan, lp_get_project(plan)->v.operand[0]->v.operand[0], NULL);
	out->keywords = lp_get_select_keywords(plan)->v.keywords;
	out->projection = lp_get_projection_columns(plan);

	// Check the optional words for distinct
	keywords = lp_get_select_keywords(plan)->v.keywords;
	keyword = get_keyword_from_keywords(keywords, OPTIONAL_DISTINCT);
	if (NULL != keyword) {
		out->distinct_values = TRUE;
		out->maintain_columnwise_index = TRUE;
	}

	keyword = get_keyword_from_keywords(keywords, OPTIONAL_BOOLEAN_EXPANSION);
	if (NULL != keyword) {
		out->emit_duplication_check = TRUE;
	}
	out->stash_columns_in_keys = options->stash_columns_in_keys;
	return out;
}

void iterate_keys(PhysicalPlan *out, LogicalPlan *plan) {
	LogicalPlan	*left, *right;

	assert(LP_KEYS == plan->type);
	GET_LP(left, plan, 0, LP_KEY);
	out->iterKeys[out->total_iter_keys] = left->v.key;
	out->total_iter_keys++;
	if (NULL != plan->v.operand[1]) {
		GET_LP(right, plan, 1, LP_KEYS);
		iterate_keys(out, right);
	}
}

LogicalPlan *walk_where_statement(PhysicalPlanOptions *options, LogicalPlan *stmt, LogicalPlan *parent) {

	if (NULL == stmt)
		return NULL;
	assert(LP_UNARY_LAST != stmt->type);
	if (stmt->type >= LP_ADDITION && stmt->type <= LP_BOOLEAN_NOT_IN) {
		stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0], stmt);
		stmt->v.operand[1] = walk_where_statement(options, stmt->v.operand[1], stmt);
	} else if (stmt->type >= LP_FORCE_NUM && stmt->type < LP_UNARY_LAST) {
		stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0], stmt);
	} else {
		PhysicalPlanOptions	curr_plan;
		PhysicalPlan		*cur, *out;
		SqlTableAlias		*table_alias;
		PhysicalPlan		*new_plan;
		LogicalPlan		*cur_lp_key, *lp_key;

		switch(stmt->type) {
		case LP_KEY:
			/* No action */
			break;
		case LP_DERIVED_COLUMN:
			/* No action */
			break;
		case LP_WHERE:
			stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0], stmt);
			break;
		case LP_COLUMN_ALIAS:
			// Check if this value is a parent reference; if so, mark this as deferred
			out = options->parent;
			cur = out;
			UNPACK_SQL_STATEMENT(table_alias, stmt->v.column_alias->table_alias, table_alias);
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
			if (NULL == cur)
				WARNING(CUSTOM_ERROR, "Problem resolving owner for deferred plan; undefined behavior");
			/* No action */
			break;
		case LP_VALUE:
			/* No action */
			break;
		case LP_INSERT:
		case LP_SET_OPERATION:
			// Generate a separate physical plan for this sub-query.
			curr_plan = *options;
			curr_plan.stash_columns_in_keys = TRUE;
			new_plan = generate_physical_plan(stmt, &curr_plan);
			if (NULL == new_plan)
				return NULL;
			if ((LP_BOOLEAN_IN == parent->type) || (LP_BOOLEAN_NOT_IN == parent->type)) {
				// Create a reference to the output key in the column list
				// (used later by "tmpl_print_expression.ctemplate" for the LP_BOOLEAN_IN/LP_BOOLEAN_NOT_IN cases)
				MALLOC_LP_2ARGS(lp_key, LP_KEY);
				cur_lp_key = lp_get_output_key(stmt);
				lp_key->v.key = cur_lp_key->v.key;
				stmt = lp_key;
			}
			break;
		case LP_FUNCTION_CALL:
		case LP_COLUMN_LIST:
		case LP_CASE:
		case LP_CASE_STATEMENT:
		case LP_CASE_BRANCH:
		case LP_CASE_BRANCH_STATEMENT:
			stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0], stmt);
			stmt->v.operand[1] = walk_where_statement(options, stmt->v.operand[1], stmt);
			break;
		case LP_TABLE:
			// This should never happen; fall through to error case
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			return NULL;
		}
	}
	return stmt;
}
