/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
	SqlStatement *set_operation_stmt;
	SqlSelectStatement *select_stmt, *select_stmt2;
	LogicalPlan *options, *set_operation, *plans, *key, *set_plans[2], *output_keys[2];
	SqlSetOperation *set_operation_sql;

	UNPACK_SQL_STATEMENT(select_stmt, stmt, select);

	assert(select_stmt->set_operation != NULL);

	// Get plans for each of the sub plans
	set_operation_stmt =  select_stmt->set_operation;
	UNPACK_SQL_STATEMENT(set_operation_sql, set_operation_stmt, set_operation);
	assert(stmt == set_operation_sql->operand[0]);
	UNPACK_SQL_STATEMENT(select_stmt2, set_operation_sql->operand[1], select);
	// otherwise this will recurse, restore it at the  end of this function for cleanup
	select_stmt->set_operation = NULL;

	set_plans[0] = generate_logical_plan(set_operation_sql->operand[0], plan_id);
	set_plans[1] = generate_logical_plan(set_operation_sql->operand[1], plan_id);

	// These should share the output key
	output_keys[0] = lp_get_output_key(set_plans[0]);
	output_keys[1] = lp_get_output_key(set_plans[1]);
	output_keys[1]->v.key->key_num = output_keys[0]->v.key->key_num;
	output_keys[1]->v.key->random_id = output_keys[0]->v.key->random_id;

	MALLOC_LP(set_operation, LP_SET_OPERATION);
	options = MALLOC_LP(set_operation->v.operand[0], LP_SET_OPTION);
	plans = MALLOC_LP(set_operation->v.operand[1], LP_PLANS);
	plans->v.operand[0] = set_plans[0];
	plans->v.operand[1] = set_plans[1];

	key = MALLOC_LP(options->v.operand[0], LP_SET_UNION);

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
	select_stmt->set_operation = set_operation_stmt;
	return set_operation;
}
