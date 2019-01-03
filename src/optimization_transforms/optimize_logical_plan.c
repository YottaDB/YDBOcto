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
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/**
 * plan should be a LP_TABLE_JOIN
 */
LogicalPlan *join_tables(LogicalPlan *root, LogicalPlan *plan) {
	LogicalPlan *next = NULL, *table_plan;
	LogicalPlan *keys = NULL, *where, *criteria, *cur_lp_key = NULL;
	LogicalPlan *sub;
	LogicalPlan *first_key;
	SqlTable *table;
	SqlTableAlias *table_alias;
	SqlColumn *key_columns[MAX_KEY_COUNT];
	int max_key, cur_key, unique_id;
	if(plan->type != LP_TABLE_JOIN || plan->v.operand[0] == NULL)
		return plan;
	if(plan->v.operand[0]->type == LP_INSERT) {
		// This plan needs to be inserted as a physical plan
		//  Leave it alone here, and let the physical planner grab it
		plan->counter = root->counter;
		sub = optimize_logical_plan(plan->v.operand[0]);
		root->counter = plan->counter;
		// Append that plans output keys to my list of keys
		GET_LP(cur_lp_key, sub, 1, LP_OUTPUT);
		GET_LP(cur_lp_key, cur_lp_key, 0, LP_KEY);
		lp_insert_key(root, cur_lp_key);
		return plan;
	}
	GET_LP(table_plan, plan, 0, LP_TABLE);
	if(plan->v.operand[1] != NULL) {
		GET_LP(next, plan, 1, LP_TABLE_JOIN);
	}
	where = lp_get_select(root);
	GET_LP(criteria, where, 1, LP_CRITERIA);
	GET_LP(keys, criteria, 0, LP_KEYS);
	first_key = keys;
	// Drill down to the bottom of the keys
	while(keys->v.operand[1] != NULL) {
		GET_LP(keys, keys, 1, LP_KEYS);
	}
	// If we drilled down somewhat, make sure we start on a fresh "key"
	if(keys->v.operand[0] != NULL) {
		MALLOC_LP(keys->v.operand[1], LP_KEYS);
		keys = keys->v.operand[1];
	}
	if(table_plan->type == LP_TABLE) {
		table_alias = table_plan->v.table_alias;
		UNPACK_SQL_STATEMENT(table, table_alias->table, table);
		unique_id = table_alias->unique_id;
		memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
		max_key = get_key_columns(table, key_columns);
		for(cur_key = 0; cur_key <= max_key; cur_key++) {
			cur_lp_key = MALLOC_LP(keys->v.operand[0], LP_KEY);
			cur_lp_key->v.key = (SqlKey*)malloc(sizeof(SqlKey));
			memset(cur_lp_key->v.key, 0, sizeof(SqlKey));
			cur_lp_key->v.key->column = key_columns[cur_key];
			cur_lp_key->v.key->key_num = cur_key;
			cur_lp_key->v.key->random_id = unique_id;
			cur_lp_key->v.key->table = table;
			cur_lp_key->v.key->type = LP_KEY_ADVANCE;
			cur_lp_key->v.key->owner = table_plan;
			if(cur_key != max_key) {
				MALLOC_LP(keys->v.operand[1], LP_KEYS);
				keys = keys->v.operand[1];
			}
		}
	} else if(table_plan->type == LP_INSERT) {
		// Else, we read from the output of the previous statement statement as a key
		GET_LP(cur_lp_key, table_plan, 1, LP_KEY);
		keys->v.operand[0] = lp_copy_plan(cur_lp_key);
	} else {
		assert(FALSE);
	}
	if(next)
		join_tables(root, next);
	return plan;
}

LogicalPlan *optimize_logical_plan(LogicalPlan *plan) {
	LogicalPlan *select, *table_join, *where, *t, *left, *right;
	SqlKey *key;
	int result, i1, i2;
	// First, "join" all the tables; we should do a search here to find the
	//  optimal join order
	select = lp_get_select(plan);
	GET_LP(table_join, select, 0, LP_TABLE_JOIN);
	join_tables(plan, table_join);

	// If there are no "OR" or "AND" statements, fix key values
	where = lp_get_select_where(plan);
	while(TRUE) {
		// Handle an equality; if one side is a constant, fix the other
		// If both sides are keys, fix the one which occurs first to the second
		//  Note: the order of the keys is part of the optimization phase
		//  prior to this
		if(where == NULL)
			break;
		t = where->v.operand[0];
		if(t == NULL)
			break;
		if(t->type != LP_BOOLEAN_EQUALS)
			break;
		left = t->v.operand[0];
		if(!(left->type == LP_VALUE || left->type == LP_COLUMN_ALIAS))
			break;
		right = t->v.operand[1];
		if(!(right->type == LP_VALUE || right->type == LP_COLUMN_ALIAS))
			break;
		// When we come out of this branching tree, left will be the value
		//  that should be fixed too, and right the value to fix it too
		if(right->type == LP_COLUMN_ALIAS && left->type == LP_COLUMN_ALIAS) {
			// Both are column references; find which occurs first
			i1 = lp_get_key_index(plan, left);
			i2 = lp_get_key_index(plan, right);
			if(i1 == -1 || i2 == -1) {
				break;
			}
			if(i2 > i1) {
				t = left;
				left = right;
				right = t;
			}
		} else if(right->type == LP_COLUMN_ALIAS) {
			// The left is a value, right a column, swap'em
			t = left;
			left = right;
			right = t;
		} else if(left->type == LP_VALUE) {
			// This something like 5=5; dumb and senseless, but the M
			//  compiler will optimize it away. Nothing we can do here
			break;
		}
		key = lp_get_key(plan, left);
		if(key == NULL)
			break;
		result = lp_opt_fix_key_to_const(plan, key, right);
		assert(result == TRUE);
		break;
	}
	return plan;
};
