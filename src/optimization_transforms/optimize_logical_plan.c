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
	LogicalPlan *sub, *sub1, *sub2;
	LogicalPlan *first_key;
	SqlTable *table;
	SqlTableAlias *table_alias;
	SqlColumn *key_columns[MAX_KEY_COUNT];
	int max_key, cur_key, unique_id;
	if(plan->type != LP_TABLE_JOIN || plan->v.operand[0] == NULL)
		return plan;
	sub = plan->v.operand[0];
	if(sub->type == LP_SET_OPERATION) {
		sub1 = optimize_logical_plan(sub->v.operand[0]);
		sub2 = optimize_logical_plan(sub->v.operand[1]);
		// Each of the sub plans should have the same output key, so we can
		//  grab from either
		GET_LP(cur_lp_key, sub1, 1, LP_OUTPUT);
		GET_LP(cur_lp_key, cur_lp_key, 0, LP_KEY);
		lp_insert_key(root, cur_lp_key);
		return plan;
	}
	if(plan->v.operand[1] != NULL) {
		GET_LP(next, plan, 1, LP_TABLE_JOIN);
	}
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
		if(next)
			join_tables(root, next);
		return plan;
	}
	GET_LP(table_plan, plan, 0, LP_TABLE);
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

LogicalPlan *lp_generate_xref_plan(LogicalPlan *plan, SqlTable *table, SqlColumn *column, int unique_id);

/**
 * Returns the keys corresponding to the cross reference for column in table, and updates
 * the LP_TABLE_JOIN of plan to include the plan which needs to be execute to generate the cross
 * reference
 */
LogicalPlan *lp_generate_xref_keys(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias) {
	LogicalPlan *root, *keys, *cur_lp_key, *table_join, *lp_table_alias, *lp_output_key;
	int cur_key, max_key, unique_id;
	SqlColumn *key_columns[MAX_KEY_COUNT], *column;
	SqlKey *output_key;

	unique_id = table_alias->unique_id;
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	// Scan through and replace the table
	table_join = lp_get_table_join(plan);
	do {
		GET_LP(lp_table_alias, table_join, 0, LP_TABLE);
		if(lp_table_alias->v.table_alias->unique_id == table_alias->unique_id)
			break;
		GET_LP(table_join, table_join, 1, LP_TABLE_JOIN);
	} while(table_join != NULL);
	assert(table_join != NULL);
	/// TODO: free the old table
	table_join->v.operand[0] = lp_generate_xref_plan(plan, table, column, unique_id);
	lp_output_key = lp_get_output_key(table_join->v.operand[0]);
	output_key = lp_output_key->v.key;
	output_key->cross_reference_column_alias = column_alias;


	keys = MALLOC_LP(root, LP_KEYS);
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
		cur_lp_key->v.key->cross_reference_output_key = output_key;
		if(cur_key != max_key) {
			MALLOC_LP(keys->v.operand[1], LP_KEYS);
			keys = keys->v.operand[1];
		}
	}
	// Replace references in the original plan
	//lp_replace_derived_table_references(plan, plan, table_alias);

	return root;
}

LogicalPlan *lp_generate_xref_plan(LogicalPlan *plan, SqlTable *table, SqlColumn *column, int unique_id) {
	LogicalPlan *root, *project, *output, *column_list, *select, *cur, *lp_cla;
	LogicalPlan *table_join, *criteria, *lp_output_key, *lp_table, *select_options, *lp_keywords;
	SqlColumnAlias *cla;
	SqlTableAlias *table_alias;
	SqlStatement *table_alias_statement;
	SqlColumn *key_columns[MAX_KEY_COUNT];
	SqlOptionalKeyword *keywords;
	SqlKey *output_key;
	int cur_key, max_key;

	// Setup the output key
	output_key = (SqlKey*)malloc(sizeof(SqlKey));
	memset(output_key, 0, sizeof(SqlKey));
	output_key->table = table;
	output_key->column = column;
	output_key->random_id = unique_id;
	output_key->is_cross_reference_key = TRUE;

	MALLOC_LP(root, LP_INSERT);
	project = MALLOC_LP(root->v.operand[0], LP_PROJECT);
	output = MALLOC_LP(root->v.operand[1], LP_OUTPUT);
	column_list = MALLOC_LP(project->v.operand[0], LP_COLUMN_LIST);
	select = MALLOC_LP(project->v.operand[1], LP_SELECT);

	// Setup the table alias that will be shared for all columns
	SQL_STATEMENT(table_alias_statement, table_alias_STATEMENT);
	MALLOC_STATEMENT(table_alias_statement, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_statement, table_alias);
	/// TODO: copy table so we can more easily clean? Fill out other fields?
	PACK_SQL_STATEMENT(table_alias->table, table, table);
	table_alias->unique_id = get_plan_unique_number(plan);
	table_alias->alias = copy_sql_statement(table->tableName);

	// Populate column list with a the column, followed by all the keys for this table
	cur = MALLOC_LP(column_list->v.operand[0], LP_WHERE);
	lp_cla = MALLOC_LP(cur->v.operand[0], LP_COLUMN_ALIAS);
	// This is used to pass information about types to the functions which output the final result
	// to the user; this table should never get there, so leave it out
	//MALLOC_LP(cur->v.operand[1], LP_COLUMN_LIST_ALIAS);
	lp_cla->v.column_alias = (SqlColumnAlias*)malloc(sizeof(SqlColumnAlias));
	memset(lp_cla->v.column_alias, 0, sizeof(SqlColumnAlias));
	cla = lp_cla->v.column_alias;
	/// TODO: copy column so we can more easily clean things up?
	PACK_SQL_STATEMENT(cla->column, column, column);
	cla->table_alias = table_alias_statement;
	// Get a list of key columns
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);
	for(cur_key = 0; cur_key <= max_key; cur_key++) {
		MALLOC_LP(column_list->v.operand[1], LP_COLUMN_LIST);
		column_list = column_list->v.operand[1];
		cur = MALLOC_LP(column_list->v.operand[0], LP_WHERE);
		lp_cla = MALLOC_LP(cur->v.operand[0], LP_COLUMN_ALIAS);
		// This is used to pass information about types to the functions which output the final result
		// to the user; this table should never get there, so leave it out
		//MALLOC_LP(cur->v.operand[1], LP_COLUMN_LIST_ALIAS);
		lp_cla->v.column_alias = (SqlColumnAlias*)malloc(sizeof(SqlColumnAlias));
		memset(lp_cla->v.column_alias, 0, sizeof(SqlColumnAlias));
		cla = lp_cla->v.column_alias;
		/// TODO: copy column so we can more easily clean things up?
		PACK_SQL_STATEMENT(cla->column, key_columns[cur_key], column);
		cla->table_alias = table_alias_statement;
	}

	// Generate a normal table join for SELECT, then populate CRITERIA and KEYS
	table_join = MALLOC_LP(select->v.operand[0], LP_TABLE_JOIN);
	criteria = MALLOC_LP(select->v.operand[1], LP_CRITERIA);
	lp_table = MALLOC_LP(table_join->v.operand[0], LP_TABLE);
	lp_table->v.table_alias = table_alias;
	MALLOC_LP(criteria->v.operand[0], LP_KEYS);
	select_options = MALLOC_LP(criteria->v.operand[1], LP_SELECT_OPTIONS);
	MALLOC_LP(select_options->v.operand[0], LP_WHERE);
	lp_keywords = MALLOC_LP(select_options->v.operand[1], LP_KEYWORDS);
	// Insert a keyword indicating that we are building an index
	lp_keywords->v.keywords = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
	memset(lp_keywords->v.keywords, 0, sizeof(SqlOptionalKeyword));
	keywords = lp_keywords->v.keywords;
	dqinit(keywords);
	keywords->keyword = OPTIONAL_POPULATE_INDEX;

	// Select an LP_KEY to output things to that is correct
	lp_output_key = MALLOC_LP(output->v.operand[0], LP_KEY);
	lp_output_key->v.key = output_key;

	// Optimize this new plan
	optimize_logical_plan(root);
	return root;
}

LogicalPlan *optimize_logical_plan(LogicalPlan *plan) {
	LogicalPlan *select, *table_join, *where, *t, *left, *right;
	LogicalPlan *first_key, *last_key, *before_first_key, *keys, *before_last_key;
	LogicalPlan *xref_keys;
	SqlKey *key;
	SqlColumn *column;
	SqlColumnAlias *column_alias;
	SqlTable *table;
	SqlTableAlias *table_alias, *table_alias2;
	SqlColumnList *column_list;
	SqlColumnListAlias *column_list_alias;
	SqlValue *value;
	int result, i1, i2;

	first_key = NULL;
	last_key = NULL;
	before_first_key = NULL;
	before_last_key = NULL;
	keys = NULL;

	if(plan->type == LP_SET_OPERATION) {
		optimize_logical_plan(plan->v.operand[1]->v.operand[0]);
		optimize_logical_plan(plan->v.operand[1]->v.operand[1]);
		return plan;
	}

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
			if(i1 == -1 && i2 == -1) {
				break;
			}
			// If the key is in the same table as the temporary value, we can't do anything
			if(i1 == -1 || i2 == -1) {
				UNPACK_SQL_STATEMENT(table_alias, left->v.column_alias->table_alias, table_alias);
				UNPACK_SQL_STATEMENT(table_alias2, right->v.column_alias->table_alias, table_alias);
				if(table_alias->unique_id == table_alias2->unique_id)
					break;
			}
			// If the temporary value is a key from a compound statement, we can't do anything
			if(i1 == -2 || i2 == -2) {
				break;
			}
			/// TODO: when we get xref table, this may need to be revisited
			// For now, we know the keys will be ordered if the left hand key will
			// be the variant key
			if(i2 == -1) {
				// Make sure the table which owns the key gets sorted second
				if(i1 == -1) {
					// Left key wasn't the key, so it's the constant value
					// Find the table for the right key
					key = lp_get_key(plan, right);
					assert(FALSE);
				} else {
					key = lp_get_key(plan, left);
				}
				// Find the first part of the key that has the same
				// random_id
				before_first_key = lp_get_criteria(plan);
				first_key = keys = lp_get_keys(plan);
				do {
					GET_LP(t, first_key, 0, LP_KEY);
					if(t->v.key->random_id == key->random_id) {
						break;
					}
					if(first_key->v.operand[1] == NULL) {
						break;
					}
					before_first_key = first_key;
					GET_LP(first_key, first_key, 1, LP_KEYS);
				} while(TRUE);
				// Find the last key
				before_last_key = before_first_key;
				last_key = first_key;
				do {
					GET_LP(t, last_key, 0, LP_KEY);
					if(t->v.key->random_id != key->random_id) {
						break;
					}
					if(last_key->v.operand[1] == NULL) {
						break;
					}
					before_last_key = last_key;
					GET_LP(last_key, last_key, 1, LP_KEYS);
				} while(TRUE);
				// Move this key to the end of the statement
				if(before_first_key->type == LP_CRITERIA) {
					before_first_key->v.operand[0] = last_key;
				} else {
					assert(before_first_key->type == LP_KEYS);
					before_first_key->v.operand[1] = last_key;
				}
				// Drill down until we get to the last key
				while(last_key->v.operand[1] != NULL) {
					GET_LP(last_key, last_key, 1, LP_KEYS);
				}
				last_key->v.operand[1] = first_key;
				before_last_key->v.operand[1] = NULL;
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
		if(key == NULL) {
			// TODO: we could insert a cross reference here
			// Get the table alias and column for left
			column_alias = left->v.column_alias;
			UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias, table_alias);
			/// TODO: how do we handle triggers on generated tables?
			if(table_alias->table->type != table_STATEMENT)
				break;
			UNPACK_SQL_STATEMENT(table, table_alias->table, table);
			if(column_alias->column->type != column_STATEMENT) {
				UNPACK_SQL_STATEMENT(column_list_alias, column_alias->column, column_list_alias);
				UNPACK_SQL_STATEMENT(column_list, column_list_alias->column_list, column_list);
				UNPACK_SQL_STATEMENT(column_alias, column_list->value, column_alias);
			}
			UNPACK_SQL_STATEMENT(column, column_alias->column, column);
			// Remove all keys for the table alias
			before_first_key = lp_get_criteria(plan);
			first_key = keys = lp_get_keys(plan);
			do {
				GET_LP(t, first_key, 0, LP_KEY);
				if(t->v.key->random_id == table_alias->unique_id) {
					break;
				}
				if(first_key->v.operand[1] == NULL) {
					break;
				}
				before_first_key = first_key;
				GET_LP(first_key, first_key, 1, LP_KEYS);
			} while(TRUE);
			// Find the last key
			last_key = first_key;
			before_last_key = first_key;
			do {
				GET_LP(t, last_key, 0, LP_KEY);
				if(t->v.key->random_id != table_alias->unique_id) {
					break;
				}
				before_last_key = last_key;
				if(last_key->v.operand[1] == NULL) {
					break;
				}
				GET_LP(last_key, last_key, 1, LP_KEYS);
			} while(TRUE);
			// Generate the new key structure
			// First, insert a new key corresponding to the column in question
			xref_keys = lp_make_key(column_alias);
			key = xref_keys->v.key;
			if(before_first_key->type == LP_CRITERIA) {
				MALLOC_LP(before_first_key->v.operand[0], LP_KEYS);
				before_first_key = before_first_key->v.operand[0];
			} else {
				MALLOC_LP(before_first_key->v.operand[1], LP_KEYS);
				before_first_key = before_first_key->v.operand[1];
			}
			before_first_key->v.operand[0] = xref_keys;
			xref_keys = lp_generate_xref_keys(plan, table, column_alias, table_alias);
			before_first_key->v.operand[1] = xref_keys;
			if(before_last_key->v.operand[1] != NULL) {
				xref_keys->v.operand[1] = before_last_key->v.operand[1];
			}
		}
		result = lp_opt_fix_key_to_const(plan, key, right);
		assert(result == TRUE);
		break;
	}
	return plan;
};
