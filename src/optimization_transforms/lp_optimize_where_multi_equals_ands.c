/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

void lp_optimize_where_multi_equals_ands(LogicalPlan *plan, LogicalPlan *where, SqlTableAlias *right_table_alias,
					 boolean_t num_outer_joins) {
	int *	     key_unique_id_array; // keys_unique_id_ordering[unique_id] = index in the ordered list
	int	     max_unique_id;	  // 1 more than maximum possible table_id/unique_id
	int	     i;
	LogicalPlan *cur, *lp_key;
	SqlKey *     key;

	max_unique_id = config->plan_id; /* similar logic exists in "lp_optimize_cross_join()" */
	key_unique_id_array = octo_cmalloc(memory_chunks, sizeof(int) * max_unique_id); // guarantees 0-initialized memory */
	// Look at the sorting of the keys in the plan ordering.
	// When selecting which to fix, always pick the key with the higher unique_id.
	// This should ensure that we will always have the right sequence.
	cur = lp_get_keys(plan);
	i = 1; // start at non-zero value since 0 is a special value indicating key is not known to the current query/sub-query
	       // (e.g. comes in from parent query). Treat 0 as the highest possible unique_id in the function
	       // "lp_optimize_where_multi_equals_ands_helper".
	while (NULL != cur) {
		GET_LP(lp_key, cur, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		// This will end up filling the table with "lowest" id, but since it'll be sequential,
		// that will still be just about right
		assert(0 < key->unique_id);
		assert(key->unique_id < max_unique_id);
		key_unique_id_array[key->unique_id] = i;
		cur = cur->v.lp_default.operand[1];
		i++;
	}
	lp_optimize_where_multi_equals_ands_helper(plan, where, key_unique_id_array, right_table_alias, num_outer_joins);
}

/* Note: "key_unique_id_array", "ptr" and "num_outer_joins" parameters are overloaded.
 * When the caller is "lp_optimize_where_multi_equals_ands()", "key_unique_id_array" is non-NULL.
 *	In this case, "ptr" parameter points to a "SqlTableAlias" structure.
 *	In this case, "num_outer_joins" parameter points to the number of outer joins.
 * When the caller is "lp_optimize_cross_join()", "key_unique_id_array" is NULL.
 *	In this case, "ptr" parameter points to a 2-dimensional "boolean_t" typed array.
 *	In this case, "num_outer_joins" parameter points to the array size of "ptr" (same size in both dimensions)
 */
LogicalPlan *lp_optimize_where_multi_equals_ands_helper(LogicalPlan *plan, LogicalPlan *where, int *key_unique_id_array, void *ptr,
							boolean_t num_outer_joins) {
	LogicalPlan *	    cur, *left, *right, *t, *keys;
	LogicalPlan *	    first_key, *before_first_key, *last_key, *before_last_key, *xref_keys;
	LogicalPlan *	    generated_xref_keys, *lp_key;
	SqlColumnList *	    column_list;
	SqlColumnListAlias *column_list_alias;
	SqlColumnAlias *    column_alias;
	SqlTableAlias *	    table_alias;
	SqlKey *	    key;
	int		    left_id, right_id;
	LogicalPlan *	    new_left, *new_right, *list;
	LPActionType	    right_type, type;
	SqlTableAlias *	    right_table_alias;

	if (LP_WHERE == where->type) {
		cur = where->v.lp_default.operand[0];
	} else {
		cur = where;
	}
	if (NULL == cur) {
		return where;
	}
	type = cur->type;
	left = cur->v.lp_default.operand[0];
	right = cur->v.lp_default.operand[1];
	switch (type) {
	case LP_BOOLEAN_AND:
		new_left = lp_optimize_where_multi_equals_ands_helper(plan, left, key_unique_id_array, ptr, num_outer_joins);
		cur->v.lp_default.operand[0] = new_left;
		new_right = lp_optimize_where_multi_equals_ands_helper(plan, right, key_unique_id_array, ptr, num_outer_joins);
		cur->v.lp_default.operand[1] = new_right;
		if (NULL == new_left) {
			if (LP_WHERE == where->type) {
				where->v.lp_default.operand[0] = new_right;
			}
			return new_right;
			break;
		}
		if (NULL == new_right) {
			if (LP_WHERE == where->type) {
				where->v.lp_default.operand[0] = new_left;
			}
			return new_left;
			break;
		}
		return where;
		break;
	case LP_BOOLEAN_EQUALS:
		break;
	case LP_BOOLEAN_IS_NOT_NULL:
		break;
	case LP_BOOLEAN_IN:
		/* Check if left side of IN is a LP_COLUMN_ALIAS. If not, we cannot do key fixing. */
		if (LP_COLUMN_ALIAS != left->type) {
			return where;
			break;
		}
		/* Check if right side of IN is a list of values (LP_VALUE). If not, we cannot do key fixing. */
		list = right;
		if (LP_COLUMN_LIST != list->type) {
			return where;
			break;
		}
		do {
			assert(LP_COLUMN_LIST == list->type);
			if (LP_VALUE != list->v.lp_default.operand[0]->type) {
				return where;
				break;
			}
			list = list->v.lp_default.operand[1];
		} while (NULL != list);
		break;
	default:
		return where; /* The only things we currently optimize are the above "case" blocks */
		break;
	}
	/* Go through and fix keys as best as we can.
	 *  1. At least one of the values is a column
	 *  2. At least one of the columns is a key
	 *  3. If one is a key, the other a column, that they're not from the same table
	 *  4. No columns are from compound statements (which don't have xref)
	 */
	switch (left->type) {
	case LP_VALUE:
		left_id = 0;
		break;
	case LP_COLUMN_ALIAS:
		UNPACK_SQL_STATEMENT(table_alias, left->v.lp_column_alias.column_alias->table_alias_stmt, table_alias);
		left_id = table_alias->unique_id;
		assert(0 < left_id);
		break;
	case LP_DERIVED_COLUMN:
		return where; /* Currently derived columns cannot be fixed. Remove this line when YDBOcto#355 is fixed */
		GET_LP(lp_key, left, 0, LP_KEY);
		right_id = lp_key->v.lp_key.key->unique_id;
		break;
	default:
		return where;
		break;
	}

	right_type = ((LP_BOOLEAN_IS_NOT_NULL == type) ? LP_VALUE : right->type);
	switch (right_type) {
	case LP_VALUE:
		right_id = 0;
		break;
	case LP_COLUMN_ALIAS:
		UNPACK_SQL_STATEMENT(table_alias, right->v.lp_column_alias.column_alias->table_alias_stmt, table_alias);
		right_id = table_alias->unique_id;
		assert(0 < right_id);
		break;
	case LP_DERIVED_COLUMN:
		return where; /* Currently derived columns cannot be fixed. Remove this line when YDBOcto#355 is fixed */
		GET_LP(lp_key, right, 0, LP_KEY);
		right_id = lp_key->v.lp_key.key->unique_id;
		break;
	case LP_COLUMN_LIST:
		assert(LP_BOOLEAN_IN == type);
		right_id = 0; /* Treat list of values as one value for key fixing purposes */
		break;
	default:
		return where;
		break;
	}
	if (NULL == key_unique_id_array) {
		/* Caller is "lp_optimize_cross_join()" */
		boolean_t *equals_id1_id2;
		int	   max_unique_id;

		max_unique_id = num_outer_joins;
		assert(max_unique_id == config->plan_id);
		assert(left_id < max_unique_id);
		assert(right_id < max_unique_id);
		equals_id1_id2 = (boolean_t *)ptr;
		equals_id1_id2[(left_id * max_unique_id) + right_id] = TRUE;
		equals_id1_id2[(right_id * max_unique_id) + left_id] = TRUE;
		return where;
	}
	if (left_id && right_id) {
		// If both column references correspond to the same table, then we cannot fix either columns/keys.
		if (left_id == right_id) {
			return where;
		}
		assert(left_id < config->plan_id);
		assert(right_id < config->plan_id);
		// If both column references correspond to tables from parent query then we cannot fix either.
		if ((0 == key_unique_id_array[left_id]) && (0 == key_unique_id_array[right_id])) {
			return where;
		}
		// If key_unique_id_array[right_id] == 0, that means the right column comes in from a parent query.
		// In that case we want to fix the left column to the right column so no swapping needed. Hence the
		// "0 != " check below (i.e. treats 0 as the highest possible unique_id; see comment in the function
		// "lp_optimize_where_multi_equals_ands()" for more details on this).
		if ((0 != key_unique_id_array[right_id]) && (key_unique_id_array[left_id] < key_unique_id_array[right_id])) {
			t = left;
			left = right;
			right = t;
		}
	} else {
		// At least one of these is a constant; just fix it
		if (right_id) {
			// The left is a value, right a column, swap'em
			if (0 == key_unique_id_array[right_id]) {
				// The right column corresponds to a unique_id coming in from the parent query.
				// In that case we cannot fix it in the sub-query. Return.
				return where;
			}
			t = left;
			left = right;
			right = t;
		} else if (left_id) {
			// The right is a value, left a column. Check if left comes in from parent query.
			// In that case, we cannot fix it. Else it is already in the right order so no swap needed
			// like was the case in the previous "if" block.
			if (0 == key_unique_id_array[left_id]) {
				// The left column corresponds to a unique_id coming in from the parent query.
				// In that case we cannot fix it in the sub-query. Return.
				return where;
			}
		} else {
			// Both left and right are values.
			assert(LP_VALUE == left->type);
			assert(LP_VALUE == right->type);
			// This something like 5=5; dumb and senseless, but the M
			//  compiler will optimize it away. Nothing we can do here.
			return where;
		}
	}
	/* Check if this is part of a JOIN ON clause (`right_table_alias` variable non-NULL). In this case, we cannot fix the key
	 * if it corresponds to a column reference i.e. LP_COLUMN_ALIAS (see YDBOcto#426 for examples that fail) from a previous
	 * table in the join list. But if the key being fixed belongs to the RIGHT side table of this JOIN then it is safe to fix.
	 * Check that. Note that the key being fixed corresponds to the variable `left` hence the check for `left->type` below.
	 */
	right_table_alias = (SqlTableAlias *)ptr;
	if ((NULL != right_table_alias) && (LP_COLUMN_ALIAS == left->type)) {
		SqlColumnAlias *column_alias;
		SqlTableAlias * column_table_alias;

		column_alias = left->v.lp_column_alias.column_alias;
		UNPACK_SQL_STATEMENT(column_table_alias, column_alias->table_alias_stmt, table_alias);
		if (column_table_alias != right_table_alias) {
			/* The column reference being fixed does not belong to the RIGHT side table of OUTER JOIN.
			 * Cannot fix this. Return unfixed plan as is.
			 */
			return where;
		}
	}
	key = lp_get_key(plan, left);
	if (LP_BOOLEAN_IS_NOT_NULL == type) {
		// Recall that right_table_alias is NULL if this is a WHERE and non-null if this is an ON clause in a join
		if (NULL != key && NULL == right_table_alias && 0 == num_outer_joins) {
			// This is of the form `WHERE primary_key IS NOT NULL`.
			// Since there are no outer joins (they are the only
			// joins that can cause NULL values even for primary
			// keys), we are guaranteed the primary key can never be NULL.
			// Therefore the condition is always true, so no need to check it for each row.
			// Therefore remove the `LP_BOOLEAN_IS_NOT_NULL` from the WHERE clause.
			if (LP_WHERE == where->type) {
				where->v.lp_default.operand[0] = NULL;
			}
			return NULL;
		} else {
			// IS NOT NULL on a non-primary column. Cannot be fixed to a constant value.
			return where;
		}
	}
	// If the left isn't a key, generate a cross reference
	if (NULL == key) {
		SqlTable *table;

		// Get the table alias and column for left
		column_alias = left->v.lp_column_alias.column_alias;
		UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
		// TODO: how do we handle triggers on generated tables?
		if (create_table_STATEMENT != table_alias->table->type) {
			/* There is no cross reference possible for an on-the-fly table constructued using the VALUES clause */
			assert(table_value_STATEMENT == table_alias->table->type);
			return where;
		}
		UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
		if (column_STATEMENT != column_alias->column->type) {
			UNPACK_SQL_STATEMENT(column_list_alias, column_alias->column, column_list_alias);
			UNPACK_SQL_STATEMENT(column_list, column_list_alias->column_list, column_list);
			UNPACK_SQL_STATEMENT(column_alias, column_list->value, column_alias);
		}
		generated_xref_keys = lp_generate_xref_keys(plan, table, column_alias, table_alias);
		if (NULL == generated_xref_keys) {
			return where;
		}
		// Remove all keys for the table alias
		before_first_key = lp_get_criteria(plan);
		first_key = keys = lp_get_keys(plan);
		do {
			GET_LP(t, first_key, 0, LP_KEY);
			if (t->v.lp_key.key->unique_id == table_alias->unique_id) {
				break;
			}
			if (NULL == first_key->v.lp_default.operand[1]) {
				break;
			}
			before_first_key = first_key;
			GET_LP(first_key, first_key, 1, LP_KEYS);
		} while (TRUE);
		// Find the last key
		last_key = first_key;
		before_last_key = first_key;
		do {
			GET_LP(t, last_key, 0, LP_KEY);
			if (t->v.lp_key.key->unique_id != table_alias->unique_id) {
				break;
			}
			before_last_key = last_key;
			if (NULL == last_key->v.lp_default.operand[1]) {
				break;
			}
			GET_LP(last_key, last_key, 1, LP_KEYS);
		} while (TRUE);
		// Generate the new key structure
		// First, insert a new key corresponding to the column in question
		xref_keys = lp_make_key(column_alias);
		key = xref_keys->v.lp_key.key;
		key->is_cross_reference_key = TRUE;
		key->cross_reference_column_alias = column_alias;
		if (LP_CRITERIA == before_first_key->type) {
			MALLOC_LP_2ARGS(before_first_key->v.lp_default.operand[0], LP_KEYS);
			before_first_key = before_first_key->v.lp_default.operand[0];
		} else {
			MALLOC_LP_2ARGS(before_first_key->v.lp_default.operand[1], LP_KEYS);
			before_first_key = before_first_key->v.lp_default.operand[1];
		}
		before_first_key->v.lp_default.operand[0] = xref_keys;
		xref_keys = generated_xref_keys;
		assert(NULL != xref_keys);
		before_first_key->v.lp_default.operand[1] = xref_keys;
		/* Note: It is possible the primary key for the table comprises of multiple columns in which case
		 * we need to scan down the xref_keys list (through v.operand[1]) until we hit the end, i.e. we
		 * cannot assume there is only one column comprising the primary key. Hence the for loop below.
		 */
		for (; NULL != xref_keys->v.lp_default.operand[1]; xref_keys = xref_keys->v.lp_default.operand[1])
			;
		if (NULL != before_last_key->v.lp_default.operand[1]) {
			xref_keys->v.lp_default.operand[1] = before_last_key->v.lp_default.operand[1];
		}
	}
	/* See if we can fix the key (`left` variable) to `right` variable. Note that we can't fix keys that are already fixed,
	 * or keys that are part of a cross reference iteration. Hence the `if` check below.
	 */
	if ((LP_KEY_ADVANCE == key->type) && (NULL == key->cross_reference_output_key)) {
		boolean_t remove_lp_boolean_equals;

		key->fixed_to_value = right;
		key->type = LP_KEY_FIX;
		/* Now that a key has been fixed to a constant (or another key), see if we can eliminate this LP_BOOLEAN_EQUALS
		 * from the WHERE or ON clause expression as it will otherwise result in a redundant IF check in the generated
		 * M code. We cannot do this removal in case this is the WHERE clause and OUTER JOINs exist (it is okay to do
		 * this if it is the ON clause and OUTER JOINs exist) as we need == check for this WHERE clause to be generated
		 * in the M code for correctness of query results. If we can remove it safely, there is still a subtlety involved
		 * with keys from parent queries which is handled further below.
		 */
		remove_lp_boolean_equals = ((NULL != right_table_alias) || (0 == num_outer_joins));
		if (remove_lp_boolean_equals) {
			if (left_id && right_id && ((0 == key_unique_id_array[left_id]) || (0 == key_unique_id_array[right_id]))) {
				/* Both are columns and one of them belongs to a parent query. In that case, do not eliminate this
				 * altogether from the WHERE/ON clause as that is relied upon in "generate_physical_plan()". Keep
				 * it in an alternate list (where->v.lp_default.operand[1]), use it in "generate_physical_plan()"
				 * (in order to ensure deferred plans get identified properly) and then switch back to
				 * "where->v.lp_default.operand[0]" for generating the plan M code which has this redundant check
				 * of a fixed key eliminated from the IF condition. This way the generated M code is optimized
				 * (in terms of not having a redundant equality check) even for plans with references to parent
				 * queries. It is okay to use where->v.lp_default.operand[1] for the alternate list because it is
				 * guaranteed to be NULL (asserted in caller function "lp_optimize_where_multi_equals_ands()").
				 */
				LogicalPlan *where2, *opr1, *lp;

				where2 = lp_get_select_where(plan);
				opr1 = where2->v.lp_default.operand[1];
				if (NULL == opr1) {
					lp = cur;
				} else {
					MALLOC_LP_2ARGS(lp, LP_BOOLEAN_AND);
					lp->v.lp_default.operand[0] = opr1;
					lp->v.lp_default.operand[1] = cur;
				}
				where2->v.lp_default.operand[1] = lp;
			}
			if (LP_WHERE == where->type) {
				where->v.lp_default.operand[0] = NULL;
			}
			where = NULL;
		} else {
			/* Do not remove this LP_BOOLEAN_EQUALS from the WHERE clause due to the presence of OUTER JOINs */
		}
	}
	return where;
}
