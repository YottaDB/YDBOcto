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
	LogicalPlan *insert, *project, *column_list, *select, *dst, *dst_key, *key;
	LogicalPlan *criteria, *table, *keys, *where, *order_by, *select_options;
	LogicalPlan *set_plans[2], *t;
	SqlColumnListAlias *list;
	SqlSetOperation *set_operation;
	SqlTableAlias *table_alias;

	UNPACK_SQL_STATEMENT(select_stmt, stmt, select);

	assert(select_stmt->set_operation != NULL);

	// Get plans for each of the sub plans
	set_operation_stmt =  select_stmt->set_operation;
	UNPACK_SQL_STATEMENT(set_operation, set_operation_stmt, set_operation);
	assert(stmt == set_operation->operand[0]);
	UNPACK_SQL_STATEMENT(select_stmt2, set_operation->operand[1], select);
	// otherwise this will recurse, restore it at the  end of this function for cleanup
	select_stmt->set_operation = NULL;

	// We should somehow inform each plan to stash the keys it iterates on
	//  in the output variable, rather than putting the value

	set_plans[0] = generate_logical_plan(set_operation->operand[0], plan_id);
	set_plans[1] = generate_logical_plan(set_operation->operand[1], plan_id);

	MALLOC_LP(insert, LP_INSERT);
	project = MALLOC_LP(insert->v.operand[0], LP_PROJECT);
	select = MALLOC_LP(project->v.operand[1], LP_SELECT);
	MALLOC_LP(select->v.operand[0], LP_TABLE_JOIN);
	criteria = MALLOC_LP(select->v.operand[1], LP_CRITERIA);
	keys = MALLOC_LP(criteria->v.operand[0], LP_KEYS);
	select_options = MALLOC_LP(criteria->v.operand[1], LP_SELECT_OPTIONS);
	where = MALLOC_LP(select_options->v.operand[0], LP_WHERE);
	insert->counter = plan_id;
	dst = MALLOC_LP(insert->v.operand[1], LP_OUTPUT);
	dst_key = MALLOC_LP(dst->v.operand[0], LP_KEY);
	dst_key->v.key = (SqlKey*)malloc(sizeof(SqlKey));
	memset(dst_key->v.key, 0, sizeof(SqlKey));
	dst_key->v.key->random_id = get_plan_unique_number(insert);
	where->v.operand[1] = NULL;

	key = MALLOC_LP(keys->v.operand[0], LP_KEY_UNION);
	// Setup the keys to be a set operation
	switch(set_operation->type) {
	case SET_UNION:
		key->type = LP_KEY_UNION;
		break;
	case SET_UNION_ALL:
		key->type = LP_KEY_UNION_ALL;
		break;
	case SET_EXCEPT:
		key->type = LP_KEY_EXCEPT;
		break;
	case SET_EXCEPT_ALL:
		key->type = LP_KEY_EXCEPT;
		break;
	case SET_INTERSECT:
		key->type = LP_KEY_INTERSECT;
		break;
	case SET_INTERSECT_ALL:
		key->type = LP_KEY_INTERSECT_ALL;
		break;
	}

	MALLOC_LP(key->v.operand[0], LP_KEY);
	key->v.operand[0]->v.key = (SqlKey*)malloc(sizeof(SqlKey));
	memset(key->v.operand[0]->v.key, 0, sizeof(SqlKey));
	key->v.operand[0]->v.key->owner = set_plans[0];
	MALLOC_LP(key->v.operand[1], LP_KEY);
	key->v.operand[1]->v.key = (SqlKey*)malloc(sizeof(SqlKey));
	memset(key->v.operand[1]->v.key, 0, sizeof(SqlKey));
	key->v.operand[1]->v.key->owner = set_plans[1];

	/// TODO: we should verify the selects being join have the same number
	///  of columns during the parsing phase
	t = lp_get_project(set_plans[0]);
	project->v.operand[0] = lp_copy_plan(t->v.operand[0]);

	// Restore set_operation for cleanup
	select_stmt->set_operation = set_operation_stmt;
	return insert;
}
