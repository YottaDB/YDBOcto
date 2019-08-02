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
	SqlOptionalKeyword *keywords, *keyword;
	LogicalPlan *keys, *table_joins, *select, *insert, *output_key, *output;
	LogicalPlan *set_operation;
	PhysicalPlan *out, *prev = NULL;

	PhysicalPlanOptions curr_plan;
	curr_plan = *options;

	assert((LP_INSERT == plan->type) || (LP_SET_OPERATION == plan->type));
	// If this is a union plan, construct physical plans for the two children
	if(plan->type == LP_SET_OPERATION) {
		out = generate_physical_plan(plan->v.operand[1]->v.operand[1], &curr_plan);
		prev = generate_physical_plan(plan->v.operand[1]->v.operand[0], &curr_plan);

		// Switch what operation the second plan does
		switch(plan->v.operand[0]->v.operand[0]->type) {
		case LP_SET_UNION:
		case LP_SET_UNION_ALL:
			out->set_operation = PP_UNION_SET;
			out->action_type = PP_PROJECT;
			break;
		case LP_SET_EXCEPT:
		case LP_SET_EXCEPT_ALL:
			out->set_operation = PP_EXCEPT_SET;
			out->action_type = PP_DELETE;
			break;
		case LP_SET_INTERSECT:
		case LP_SET_INTERSECT_ALL:
			out->set_operation = PP_INTERSECT_SET;
			out->action_type = PP_DELETE;
			break;
		default:
			assert(FALSE);
		}

		// If the SET is not an "ALL" type, we need to keep resulting rows
		//  unique
		switch(plan->v.operand[0]->v.operand[0]->type) {
		case LP_SET_UNION:
		case LP_SET_EXCEPT:
		case LP_SET_INTERSECT:
			out->distinct_values = TRUE;
			prev->distinct_values = TRUE;
			out->maintain_columnwise_index = TRUE;
			prev->maintain_columnwise_index = TRUE;
			break;
		case LP_SET_UNION_ALL:
		case LP_SET_EXCEPT_ALL:
		case LP_SET_INTERSECT_ALL:
			// Even though we don't use the index for the UNION_ALL case, we need to maintain
			// it so any parent SET operations have knowledge to act
			// In theory, we could check to see if there is nothing but UNION_ALL's, and that
			// should be a future enhacement, but it's not the case now
			out->maintain_columnwise_index = TRUE;
			prev->maintain_columnwise_index = TRUE;
			break;
		default:
			assert(FALSE);
		}
		return prev;
	}

	// Make sure the plan is in good shape
	if(lp_verify_structure(plan) == FALSE) {
		/// TODO: replace this with a real error message
		FATAL(CUSTOM_ERROR, "Bad plan!");
	}
	OCTO_CMALLOC_STRUCT(out, PhysicalPlan);
	out->parent_plan = options->parent;
	curr_plan.parent = out;

	out->next = *curr_plan.last_plan;
	if(*curr_plan.last_plan != NULL) {
		(*curr_plan.last_plan)->prev = out;
	}
	*(curr_plan.last_plan) = out;

	// Set my output key
	GET_LP(output, plan, 1, LP_OUTPUT);
	if(output->v.operand[0]->type == LP_KEY) {
		GET_LP(output_key, output, 0, LP_KEY);
		out->outputKey = output_key->v.key;
		out->is_cross_reference_key = out->outputKey->is_cross_reference_key;
	} else if(output->v.operand[0]->type == LP_TABLE) {
		out->outputKey = NULL;
		out->outputTable = output->v.operand[1]->v.table_alias;
	} else {
		assert(FALSE);
	}

	// If there is an order by, note it down
	if(output->v.operand[1]) {
		GET_LP(out->order_by, output, 1, LP_COLUMN_LIST);
	}

	// See if there are any tables we rely on in the SELECT or WHERE;
	//  if so, add them as prev records
	select = lp_get_select(plan);
	GET_LP(table_joins, select, 0, LP_TABLE_JOIN);
	do {
		// If this is a plan that doesn't have a source table,
		//  this will be null and we need to skip this step
		if(table_joins->v.operand[0] == NULL)
			break;
		if(table_joins->v.operand[0]->type == LP_INSERT) {
			// This is a sub plan, and should be inserted as prev
			GET_LP(insert, table_joins, 0, LP_INSERT);
			generate_physical_plan(insert, &curr_plan);
		} else if(table_joins->v.operand[0]->type == LP_SET_OPERATION) {
			GET_LP(set_operation, table_joins, 0, LP_SET_OPERATION);
			generate_physical_plan(set_operation, &curr_plan);
		}
		table_joins = table_joins->v.operand[1];
	} while(table_joins != NULL);

	// Iterate through the key substructures and fill out the source keys
	keys = lp_get_keys(plan);
	// All tables should have at least one key
	assert(keys != NULL);
	// Either we have some keys already, or we have a list of keys
	assert(out->total_iter_keys > 0 || keys->v.operand[0] != NULL);
	if(keys->v.operand[0])
		iterate_keys(out, keys);

	// The output key should be a cursor key

	// Is this most convenient representation of the WHERE?
	out->where = walk_where_statement(&curr_plan, lp_get_select_where(plan));
	out->projection = walk_where_statement(&curr_plan, lp_get_project(plan)->v.operand[0]->v.operand[0]);
	out->keywords = lp_get_select_keywords(plan)->v.keywords;

	out->projection = lp_get_projection_columns(plan);
	// As a temporary measure, wrap all tables in a SqlTableAlias
	//  so that we can search through them later;
	GET_LP(table_joins, select, 0, LP_TABLE_JOIN);

	// Check the optional words for distinct
	keywords = lp_get_select_keywords(plan)->v.keywords;
	keyword = get_keyword_from_keywords(keywords, OPTIONAL_DISTINCT);
	if(keyword != NULL) {
		out->distinct_values = 1;
		out->maintain_columnwise_index = 1;
	}

	keyword = get_keyword_from_keywords(keywords, OPTIONAL_PART_OF_EXPANSION);
	if(keyword != NULL) {
		out->emit_duplication_check = TRUE;
	}

	out->stash_columns_in_keys = options->stash_columns_in_keys;

	return out;
}

void iterate_keys(PhysicalPlan *out, LogicalPlan *plan) {
	LogicalPlan *left, *right;
	assert(plan->type == LP_KEYS);

	GET_LP(left, plan, 0, LP_KEY);
	out->iterKeys[out->total_iter_keys] = left->v.key;
	out->total_iter_keys++;

	if(plan->v.operand[1] != NULL) {
		GET_LP(right, plan, 1, LP_KEYS);
		iterate_keys(out, right);
	}
}

LogicalPlan *walk_where_statement(PhysicalPlanOptions *options, LogicalPlan *stmt) {
	PhysicalPlan *cur, *out;

	if(stmt == NULL)
		return NULL;

	PhysicalPlanOptions curr_plan;
	out = options->parent;
	if(stmt->type >= LP_ADDITION && stmt->type <= LP_BOOLEAN_NOT_IN) {
		stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0]);
		stmt->v.operand[1] = walk_where_statement(options, stmt->v.operand[1]);
	} else if (stmt->type >= LP_FORCE_NUM && stmt->type <= LP_BOOLEAN_NOT) {
		stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0]);
	} else {
		switch(stmt->type) {
		case LP_KEY:
			/* No action */
			break;
		case LP_DERIVED_COLUMN:
			/* No action */
			break;
		case LP_WHERE:
			stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0]);
			break;
		case LP_COLUMN_ALIAS:
			// Check if this value is a parent reference; if so, mark this as deferred
			cur = out;
			SqlTableAlias *table_alias;
			UNPACK_SQL_STATEMENT(table_alias, stmt->v.column_alias->table_alias, table_alias);
			while(cur) {
				unsigned int i = 0;
				for(; i < out->total_iter_keys; i++) {
					if(cur->iterKeys[i]->unique_id == table_alias->unique_id) {
						break;
					}
				}
				if(i != cur->total_iter_keys) {
					break;
				}
				cur->deferred_plan = TRUE;
				cur = cur->parent_plan;
			}
			if(cur == NULL) {
				WARNING(CUSTOM_ERROR, "Problem resolving owner for deferred plan; undefined behavior");
			}
			/* No action */
			break;
		case LP_VALUE:
			/* No action */
			break;
		case LP_INSERT:
		case LP_SET_OPERATION:
			// Insert this to the physical plan, then create a
			//  reference to the first item in the column list
			curr_plan = *options;
			curr_plan.stash_columns_in_keys = TRUE;
			PhysicalPlan *new_plan = generate_physical_plan(stmt, &curr_plan);
			MALLOC_LP_2ARGS(stmt, LP_KEY);
			stmt->v.key = new_plan->outputKey;
			break;
		case LP_FUNCTION_CALL:
		case LP_COLUMN_LIST:
		case LP_CASE:
		case LP_CASE_STATEMENT:
		case LP_CASE_BRANCH:
		case LP_CASE_BRANCH_STATEMENT:
			stmt->v.operand[0] = walk_where_statement(options, stmt->v.operand[0]);
			stmt->v.operand[1] = walk_where_statement(options, stmt->v.operand[1]);
			break;
		case LP_TABLE:
			// This should never happen; fall through to error case
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
	}
	return stmt;
}
