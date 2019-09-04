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

void lp_update_plan_keys(LogicalPlan *plan, SqlKey *key) {
	LogicalPlan *output_key;

	if(plan->type == LP_SET_OPERATION) {
		assert(plan->v.operand[1]->type == LP_PLANS);
		lp_update_plan_keys(plan->v.operand[1]->v.operand[0], key);
		lp_update_plan_keys(plan->v.operand[1]->v.operand[1], key);
	} else if(plan->type == LP_INSERT) {
		output_key = lp_get_output_key(plan);
		output_key->v.key->key_num = key->key_num;
		output_key->v.key->unique_id = key->unique_id;
	} else {
		assert(FALSE);
	}
}

/**
 * Generates a meta-plan for doing a KEY_<set operation>
 */
LogicalPlan *lp_generate_set_logical_plan(SqlStatement *stmt, int *plan_id) {
	SqlStatement *set_operation_stmt;
	LogicalPlan *options, *set_operation, *plans, *key, *set_plans[2], *output_key;
	LogicalPlan *cur_plan;
	SqlSetOperation *set_operation_sql;

	// Get plans for each of the sub plans
	set_operation_stmt = stmt;
	UNPACK_SQL_STATEMENT(set_operation_sql, set_operation_stmt, set_operation);

	set_plans[0] = generate_logical_plan(set_operation_sql->operand[0], plan_id);
	if (NULL == set_plans[0])
		return NULL;
	set_plans[1] = generate_logical_plan(set_operation_sql->operand[1], plan_id);
	if (NULL == set_plans[1])
		return NULL;

	// These should share the output key
	cur_plan = lp_drill_to_insert(set_plans[0]);
	output_key = lp_get_output_key(cur_plan);
	lp_update_plan_keys(set_plans[0], output_key->v.key);
	lp_update_plan_keys(set_plans[1], output_key->v.key);

	MALLOC_LP_2ARGS(set_operation, LP_SET_OPERATION);
	MALLOC_LP(options, set_operation->v.operand[0], LP_SET_OPTION);
	MALLOC_LP(plans, set_operation->v.operand[1], LP_PLANS);
	plans->v.operand[0] = set_plans[0];
	plans->v.operand[1] = set_plans[1];

	MALLOC_LP(key, options->v.operand[0], LP_SET_UNION);

	// Setup the keys to be a set operation
	switch(set_operation_sql->type) {
	case SET_UNION:
		key->type = LP_SET_UNION;
		break;
	case SET_UNION_ALL:
		key->type = LP_SET_UNION_ALL;
		break;
	case SET_EXCEPT:
		key->type = LP_SET_EXCEPT;
		break;
	case SET_EXCEPT_ALL:
		key->type = LP_SET_EXCEPT_ALL;
		break;
	case SET_INTERSECT:
		key->type = LP_SET_INTERSECT;
		break;
	case SET_INTERSECT_ALL:
		key->type = LP_SET_INTERSECT_ALL;
		break;
	}

	/// TODO: we should verify the selects being join have the same number
	///  of columns during the parsing phase

	// Restore set_operation for cleanup
	return set_operation;
}
