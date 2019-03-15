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
 * Verifies that the equals statement in equals can be "fixed", which involves checking
 * that:
 *  1. At least one of the values is a column
 *  2. At least one of the columns is a key
 *  3. If one is a key, the other a column, that they're not from the same table
 *  4. No columns are from compound statements (which don't have xref)
 *
 * This does not generate cross references if needed
 */
int lp_verify_valid_for_key_fix(LogicalPlan *plan, LogicalPlan *equals) {
	LogicalPlan *left, *right;
	int i1, i2;
	SqlTableAlias *table_alias, *table_alias2;

	if(equals == NULL)
		return FALSE;
	if(equals->type != LP_BOOLEAN_EQUALS)
		return FALSE;
	left = equals->v.operand[0];
	if(!(left->type == LP_VALUE || left->type == LP_COLUMN_ALIAS))
		return FALSE;
	right = equals->v.operand[1];
	if(!(right->type == LP_VALUE || right->type == LP_COLUMN_ALIAS))
		return FALSE;
	if(right->type == LP_COLUMN_ALIAS && left->type == LP_COLUMN_ALIAS) {
		// Both are column references; find which occurs first
		i1 = lp_get_key_index(plan, left);
		i2 = lp_get_key_index(plan, right);
		if(i1 == -1 && i2 == -1) {
			return FALSE;
		}
		// If the key is in the same table as the temporary value, we can't do anything
		if(i1 == -1 || i2 == -1) {
			UNPACK_SQL_STATEMENT(table_alias, left->v.column_alias->table_alias, table_alias);
			UNPACK_SQL_STATEMENT(table_alias2, right->v.column_alias->table_alias, table_alias);
			if(table_alias->unique_id == table_alias2->unique_id)
				return FALSE;
		}
		// If the temporary value is a key from a compound statement, we can't do anything
		if(i1 == -2 || i2 == -2) {
			return FALSE;
		}
	}
	return TRUE;
}

int lp_optimize_where_multi_equal_ands(LogicalPlan *plan, LogicalPlan *where) {
	LogicalPlan *cur, *equals, *lp_key, *left, *right, *t, *keys, *prev;
	LogicalPlan *first_key, *before_first_key, *last_key, *before_last_key, *xref_keys;
	SqlColumnList *column_list;
	SqlColumnListAlias *column_list_alias;
	SqlColumnAlias *column_alias;
	SqlTable *table;
	SqlColumn *column;
	SqlTableAlias *table_alias;
	SqlKey *key;
	int i, left_id, right_id, result;
	// keys_unique_id_ordering[unique_id] = index in the ordered list
	int key_unique_id_ordering[MAX_STR_CONST];
	cur = where->v.operand[0];
	prev = NULL;
	while(cur != NULL) {
		if(cur->type != LP_BOOLEAN_AND)
			return 0;
		// Get the one with equals
		if(cur->v.operand[0]->type == LP_BOOLEAN_EQUALS &&
				cur->v.operand[1]->type == LP_BOOLEAN_EQUALS) {
			if(prev == NULL) {
				equals = cur->v.operand[0];
				prev = cur;
			} else {
				equals = cur->v.operand[1];
				cur = NULL;
			}
		} else if(cur->v.operand[0]->type == LP_BOOLEAN_EQUALS) {
			equals = cur->v.operand[0];
			cur = cur->v.operand[1];
		} else if(cur->v.operand[1]->type == LP_BOOLEAN_EQUALS) {
			equals = cur->v.operand[1];
			cur = cur->v.operand[0];
		} else {
			return 0;
		}
		if(lp_verify_valid_for_key_fix(plan, equals) == FALSE)
			return 0;
	}
	// Look at the sorting of the keys in the plan ordering; when selecting which to fix,
	// always pick the "lower" one; this should ensure that we will always have the right
	// sequence
	cur = lp_get_keys(plan);
	i = 0;
	while(cur != NULL) {
		GET_LP(lp_key, cur, 0, LP_KEY);
		key = lp_key->v.key;
		// This will end up filling the table with "lowest" id, but since it'll be seqential,
		// that will still be just about right
		key_unique_id_ordering[key->random_id] = i;
		cur = cur->v.operand[1];
		i++;
	}
	// Go through and fix keys as best as we can
	cur = where->v.operand[0];
	prev = NULL;
	while(cur != NULL) {
		if(cur->type != LP_BOOLEAN_AND)
			return 0;
		// Get the one with equals
		if(cur->v.operand[0]->type == LP_BOOLEAN_EQUALS &&
				cur->v.operand[1]->type == LP_BOOLEAN_EQUALS) {
			if(prev == NULL) {
				equals = cur->v.operand[0];
				prev = cur;
			} else {
				equals = cur->v.operand[1];
				cur = NULL;
			}
		} else if(cur->v.operand[0]->type == LP_BOOLEAN_EQUALS) {
			equals = cur->v.operand[0];
			cur = cur->v.operand[1];
		} else if(cur->v.operand[1]->type == LP_BOOLEAN_EQUALS) {
			equals = cur->v.operand[1];
			cur = cur->v.operand[0];
		} else {
			return 0;
		}
		left = equals->v.operand[0];
		right = equals->v.operand[1];
		if(right->type == LP_COLUMN_ALIAS && left->type == LP_COLUMN_ALIAS) {
			UNPACK_SQL_STATEMENT(table_alias, right->v.column_alias->table_alias, table_alias);
			right_id = table_alias->unique_id;
			UNPACK_SQL_STATEMENT(table_alias, left->v.column_alias->table_alias, table_alias);
			left_id = table_alias->unique_id;
			if(key_unique_id_ordering[left_id] < key_unique_id_ordering[right_id]) {
				t = left;
				left = right;
				right = t;
			}
		} else {
			// At least one of these is a constant; just fix it
			if(right->type == LP_COLUMN_ALIAS) {
				// The left is a value, right a column, swap'em
				t = left;
				left = right;
				right = t;
			} else if(left->type == LP_VALUE) {
				// This something like 5=5; dumb and senseless, but the M
				//  compiler will optimize it away. Nothing we can do here
				continue;
			}
		}
		key = lp_get_key(plan, left);
		// If the left isn't a key, generate a cross reference
		if(key == NULL) {
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
	}
	return 1;
}
