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

/**
 * Generates a meta-plan for doing a KEY_<set operation>
 */
LogicalPlan *lp_generate_set_logical_plan(SqlStatement *stmt, int *plan_id) {
	SqlStatement	*set_operation_stmt;
	LogicalPlan	*options, *set_operation, *plans, *key, *set_plans[2];
	LogicalPlan	*dst, *dst_key;
	SqlSetOperation	*set_operation_sql;

	// Get plans for each of the sub plans
	set_operation_stmt = stmt;
	UNPACK_SQL_STATEMENT(set_operation_sql, set_operation_stmt, set_operation);

	set_plans[0] = generate_logical_plan(set_operation_sql->operand[0], plan_id);
	if (NULL == set_plans[0])
		return NULL;
	set_plans[1] = generate_logical_plan(set_operation_sql->operand[1], plan_id);
	if (NULL == set_plans[1])
		return NULL;

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
	MALLOC_LP(dst, options->v.operand[1], LP_OUTPUT);
	MALLOC_LP(dst_key, dst->v.operand[0], LP_KEY);
	OCTO_CMALLOC_STRUCT(dst_key->v.key, SqlKey);
	memset(dst_key->v.key, 0, sizeof(SqlKey));
	dst_key->counter = plan_id;
	dst_key->v.key->unique_id = get_plan_unique_number(dst_key);
	dst_key->v.key->type = LP_KEY_ADVANCE;
	// Restore set_operation for cleanup
	return set_operation;
}
