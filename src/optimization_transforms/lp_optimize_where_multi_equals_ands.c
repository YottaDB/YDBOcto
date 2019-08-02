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
#include "logical_plan.h"

// Below is a helper function that should be invoked only from this file hence the prototype is defined in the .c file
// and not in the .h file like is the usual case.
int lp_optimize_where_multi_equal_ands_helper(LogicalPlan *plan, LogicalPlan *where, int *key_unique_id_array);

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

	assert(NULL != equals);
	assert(LP_BOOLEAN_EQUALS == equals->type);
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
			// Both of them are columns that are not keys; we should still be able to optimize one
			// using the cross reference
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
	int		*key_unique_id_array;	// keys_unique_id_ordering[unique_id] = index in the ordered list
	int		max_unique_id;		// 1 more than maximum possible table_id/unique_id
	int		i;
	LogicalPlan	*cur, *lp_key;
	SqlKey		*key;

	max_unique_id = *plan->counter - 1;
	key_unique_id_array = octo_cmalloc(memory_chunks, sizeof(int) * max_unique_id);	// guarantees 0-initialized memory */
	// Look at the sorting of the keys in the plan ordering.
	// When selecting which to fix, always pick the key with the higher unique_id.
	// This should ensure that we will always have the right sequence.
	cur = lp_get_keys(plan);
	i = 1;	// start at non-zero value since 0 is a special value indicating key is not known to the current query/sub-query
		// (e.g. comes in from parent query). Treat 0 as the highest possible unique_id in the function
		// "lp_optimize_where_multi_equal_ands_helper".
	while(cur != NULL) {
		GET_LP(lp_key, cur, 0, LP_KEY);
		key = lp_key->v.key;
		// This will end up filling the table with "lowest" id, but since it'll be seqential,
		// that will still be just about right
		key_unique_id_array[key->unique_id] = i;
		cur = cur->v.operand[1];
		i++;
	}
	return lp_optimize_where_multi_equal_ands_helper(plan, where, key_unique_id_array);
}

int lp_optimize_where_multi_equal_ands_helper(LogicalPlan *plan, LogicalPlan *where, int *key_unique_id_array) {
	LogicalPlan *cur, *left, *right, *t, *keys;
	LogicalPlan *first_key, *before_first_key, *last_key, *before_last_key, *xref_keys;
	LogicalPlan *generated_xref_keys;
	SqlColumnList *column_list;
	SqlColumnListAlias *column_list_alias;
	SqlColumnAlias *column_alias;
	SqlTable *table;
	SqlTableAlias *table_alias;
	SqlKey *key;
	int	left_id, right_id;
	int	total_optimizations_done;

	if(where->type == LP_WHERE) {
		cur = where->v.operand[0];
	} else {
		cur = where;
	}
	if (NULL == cur)
		return 0;
	assert(LP_BOOLEAN_OR != cur->type);	/* all ORs should already be eliminated before coming here */
	if (LP_BOOLEAN_AND == cur->type)
	{
		total_optimizations_done = lp_optimize_where_multi_equal_ands_helper(plan, cur->v.operand[0], key_unique_id_array);
		total_optimizations_done += lp_optimize_where_multi_equal_ands_helper(plan, cur->v.operand[1], key_unique_id_array);
		return total_optimizations_done;
	}
	if (LP_BOOLEAN_EQUALS != cur->type)
		return 0; // The only things we currently optimize are AND and EQUALS
	// Go through and fix keys as best as we can
	if (FALSE == lp_verify_valid_for_key_fix(plan, cur))
		return 0;
	left = cur->v.operand[0];
	right = cur->v.operand[1];

	if(right->type == LP_COLUMN_ALIAS && left->type == LP_COLUMN_ALIAS) {
		UNPACK_SQL_STATEMENT(table_alias, right->v.column_alias->table_alias, table_alias);
		right_id = table_alias->unique_id;
		UNPACK_SQL_STATEMENT(table_alias, left->v.column_alias->table_alias, table_alias);
		left_id = table_alias->unique_id;
		assert(0 <= left_id);
		assert(0 <= right_id);
		assert(left_id < *plan->counter);
		assert(right_id < *plan->counter);
		// If both column references correspond to tables from parent query then we cannot fix either.
		if ((0 == key_unique_id_array[left_id]) && (0 == key_unique_id_array[right_id])) {
			return 0;
		}
		// The "0 == " check below treats 0 as the highest possible unique_id. See comment in the function
		// "lp_optimize_where_multi_equal_ands" for more details on this.
		if ((0 == key_unique_id_array[right_id])
				|| (key_unique_id_array[left_id] < key_unique_id_array[right_id])) {
			t = left;
			left = right;
			right = t;
		}
	} else {
		// At least one of these is a constant; just fix it
		if(LP_COLUMN_ALIAS == right->type) {
			// The left is a value, right a column, swap'em
			UNPACK_SQL_STATEMENT(table_alias, right->v.column_alias->table_alias, table_alias);
			right_id = table_alias->unique_id;
			if(0 == key_unique_id_array[right_id]) {
				// The right column corresponds to a unique_id coming in from the parent query.
				// In that case we cannot fix it in the sub-query. Return.
				return 0;
			}
			t = left;
			left = right;
			right = t;
		} else if(LP_COLUMN_ALIAS == left->type) {
			// The right is a value, left a column. Check if left comes in from parent query.
			// In that case, we cannot fix it. Else it is already in the right order so no swap needed
			// like was the case in the previous "if" block.
			UNPACK_SQL_STATEMENT(table_alias, left->v.column_alias->table_alias, table_alias);
			left_id = table_alias->unique_id;
			if(0 == key_unique_id_array[left_id]) {
				// The left column corresponds to a unique_id coming in from the parent query.
				// In that case we cannot fix it in the sub-query. Return.
				return 0;
			}
		} else {
			// Both left and right are values.
			assert(LP_VALUE == left->type);
			assert(LP_VALUE == right->type);
			// This something like 5=5; dumb and senseless, but the M
			//  compiler will optimize it away. Nothing we can do here.
			return 0;
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
			return 0;
		UNPACK_SQL_STATEMENT(table, table_alias->table, table);
		if(column_alias->column->type != column_STATEMENT) {
			UNPACK_SQL_STATEMENT(column_list_alias, column_alias->column, column_list_alias);
			UNPACK_SQL_STATEMENT(column_list, column_list_alias->column_list, column_list);
			UNPACK_SQL_STATEMENT(column_alias, column_list->value, column_alias);
		}
		generated_xref_keys = lp_generate_xref_keys(plan, table, column_alias, table_alias);
		if(generated_xref_keys == NULL)
			return 0;
		// Remove all keys for the table alias
		before_first_key = lp_get_criteria(plan);
		first_key = keys = lp_get_keys(plan);
		do {
			GET_LP(t, first_key, 0, LP_KEY);
			if(t->v.key->unique_id == table_alias->unique_id) {
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
			if(t->v.key->unique_id != table_alias->unique_id) {
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
		key->is_cross_reference_key = TRUE;
		if(before_first_key->type == LP_CRITERIA) {
			MALLOC_LP_2ARGS(before_first_key->v.operand[0], LP_KEYS);
			before_first_key = before_first_key->v.operand[0];
		} else {
			MALLOC_LP_2ARGS(before_first_key->v.operand[1], LP_KEYS);
			before_first_key = before_first_key->v.operand[1];
		}
		before_first_key->v.operand[0] = xref_keys;
		xref_keys = generated_xref_keys;
		assert(xref_keys != NULL);
		before_first_key->v.operand[1] = xref_keys;
		if(before_last_key->v.operand[1] != NULL) {
			xref_keys->v.operand[1] = before_last_key->v.operand[1];
		}
	}
	total_optimizations_done = lp_opt_fix_key_to_const(plan, key, right);
	return total_optimizations_done;
}
