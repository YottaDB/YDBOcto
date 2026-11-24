/****************************************************************
 *								*
 * Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	*
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
#include "template_helpers.h"

/* Test if a key column is transformed by AIM (currently boolean and date/time key columns of a READONLY table)
 *
 * Returns TRUE if "operand" (an LP_COLUMN_ALIAS) is a primary key column whose AIM cross reference holds its values
 * converted by a transformation function: a boolean or date/time key column of a READONLY table (IS_XREF_VALUE_TRANSFORMED).
 * The table's global holds such a value in a form that a condition cannot compute from the value (e.g. the date 2024-01-15
 * stored as "2024-01-15" or "2024-1-15", TRUE stored as 1 or "Y"). So the key cannot be fixed in place: only the cross
 * reference of the column, which holds the converted values, can serve a condition on it (YDBOcto#660).
 */
static boolean_t is_transformed_key_column(LogicalPlan *operand) {
	SqlColumnAlias *column_alias;
	SqlColumn      *column;
	SqlTable       *table;

	assert(LP_COLUMN_ALIAS == operand->type);
	column_alias = operand->v.lp_column_alias.column_alias;
	if (column_STATEMENT != column_alias->column->type) {
		return FALSE; /* "table.*" is not one column */
	}
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	if ((create_table_STATEMENT != column->table->type) || !IS_KEY_COLUMN(column)) {
		return FALSE; /* Not a key column of a table defined by CREATE TABLE */
	}
	UNPACK_SQL_STATEMENT(table, column->table, create_table);
	return IS_XREF_VALUE_TRANSFORMED(table, column);
}

/* Returns the KEY NUM of the primary key column "column" */
int lp_get_key_num(SqlColumn *column) {
	SqlOptionalKeyword *key_num_keyword;
	SqlValue	   *value;

	key_num_keyword = get_keyword(column, OPTIONAL_KEY_NUM);
	assert(NULL != key_num_keyword);
	UNPACK_SQL_STATEMENT(value, key_num_keyword->v, value);
	return atoi(value->v.string_literal);
}

/* Test if a comparison between columns of two tables fixes the column of the first table
 *
 * When a comparison relates a column of the table "id" to a column of another table "other_id", returns TRUE if the key
 * fixing fixes the column of "id" (to the column of "other_id"), FALSE if it fixes the column of "other_id" instead. It
 * fixes the column of the table whose FOR loop comes later in "key_unique_id_array". A column of a parent query (0 in
 * "key_unique_id_array") is as a value: it is never fixed, the other column is. The caller has made sure that the two
 * tables differ and that at least one of them belongs to this query.
 * "lp_optimize_where_multi_equals_ands_helper()" uses this to decide which column to fix, and "can_fix_key_of_table()" to
 * tell whether a comparison can fix a key of a table at all, so that both make the same choice.
 */
static boolean_t is_column_fixed_to_other_table(int *key_unique_id_array, int id, int other_id) {
	assert(id != other_id);
	assert((0 != key_unique_id_array[id]) || (0 != key_unique_id_array[other_id]));
	return !((0 != key_unique_id_array[other_id]) && (key_unique_id_array[id] < key_unique_id_array[other_id]));
}

/* Test if a comparison with a key column can fix a key of that column's table
 *
 * Returns FALSE if "other_operand", the other side of a comparison with a key column of the table "unique_id", is a column
 * of that same table, or of a table whose FOR loop comes after that table's in "key_unique_id_array": the key fixing then
 * fixes no key of the table "unique_id" with the comparison. TRUE otherwise (a value, a parameter, a column of a table whose
 * FOR loop comes before, or of a parent query, or anything else), which keeps the caller's conservative answer.
 * The choice between two tables is "is_column_fixed_to_other_table()", which the key fixing itself uses.
 */
static boolean_t can_fix_key_of_table(LogicalPlan *other_operand, int unique_id, int *key_unique_id_array) {
	SqlColumnAlias *column_alias;
	SqlTableAlias  *table_alias;
	int		other_id;

	if ((NULL == other_operand) || (LP_COLUMN_ALIAS != other_operand->type)) {
		return TRUE; /* A value, a parameter or an expression: as before, it counts */
	}
	column_alias = other_operand->v.lp_column_alias.column_alias;
	/* Not a TABLENAME.ASTERISK: comparing one with a column is ERR_TABLE_ASTERISK_SCALAR_COMPARISON */
	assert(column_STATEMENT == column_alias->column->type);
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
	other_id = table_alias->unique_id;
	if (other_id == unique_id) {
		return FALSE; /* Two columns of the same table: the key fixing fixes neither */
	}
	assert(other_id < config->plan_id);
	assert(unique_id < config->plan_id);
	if (!is_column_fixed_to_other_table(key_unique_id_array, unique_id, other_id)) {
		return FALSE; /* The other table's FOR loop comes later: its column is fixed to this one, not the other way round */
	}
	return TRUE;
}

/* Test if a condition compares one of the first key columns of a table
 *
 * Returns TRUE if one of the AND-ed comparisons of "cond" (a WHERE or ON condition) compares a primary key column of the
 * table "unique_id" whose KEY NUM is at most "max_key_num" (YDBOcto#660). Every comparison operator counts, including
 * IS NULL and a comparison with a value the key fixing cannot use: the callers only need to know that the condition may
 * constrain such a key column, and erring that way keeps the plan they would otherwise choose.
 *   - "use_key_column_xref()": a condition on a key column before the column at "key_num" may fix it in place, which
 *     makes the loops before that column run once, so a cross reference on the latter would gain nothing. The conditions
 *     are processed in order, so this looks at those not processed yet too. E.g. "WHERE id = 5 AND acc = 'X'" with keys
 *     (acc, id): the "id" condition is seen first, and "acc" is constrained.
 *   - "lp_optimize_order_by_with_limit()": see there.
 *
 * "key_unique_id_array" is the order of the tables' FOR loops ("lp_optimize_where_multi_equals_ands()"), or NULL. With
 * it, a comparison of the key column with a column of a table whose FOR loop comes after this table's does not count:
 * the key fixing fixes the key of the later table to this key column, never the other way round, so the comparison
 * leaves this table's FOR loops as they are. E.g. "FROM pa JOIN p ON p.patient_id = pa.patient_id" fixes "p.patient_id"
 * to each "pa.patient_id", and "pa" is still walked. Nor does a comparison with another column of the same table, which
 * fixes neither. Without it (NULL), every comparison counts.
 */
boolean_t lp_is_key_column_compared(LogicalPlan *cond, int unique_id, int max_key_num, int *key_unique_id_array) {
	LogicalPlan *operand;
	int	     i;

	if (NULL == cond) {
		return FALSE;
	}
	switch (cond->type) {
	case LP_WHERE: /* A JOIN's ON condition */
		return lp_is_key_column_compared(cond->v.lp_default.operand[0], unique_id, max_key_num, key_unique_id_array);
	case LP_BOOLEAN_AND:
		return lp_is_key_column_compared(cond->v.lp_default.operand[0], unique_id, max_key_num, key_unique_id_array)
		       || lp_is_key_column_compared(cond->v.lp_default.operand[1], unique_id, max_key_num, key_unique_id_array);
	case LP_BOOLEAN_EQUALS:
	case LP_BOOLEAN_IS:
	case LP_BOOLEAN_IN:
	case LP_BOOLEAN_LESS_THAN:
	case LP_BOOLEAN_GREATER_THAN:
	case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
	case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY:
	case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX:
	case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX:
		for (i = 0; i < 2; i++) {
			SqlColumnAlias *column_alias;
			SqlTableAlias  *table_alias;
			SqlColumn      *column;

			operand = cond->v.lp_default.operand[i];
			if ((NULL == operand) || (LP_COLUMN_ALIAS != operand->type)) {
				continue;
			}
			column_alias = operand->v.lp_column_alias.column_alias;
			if (column_STATEMENT != column_alias->column->type) {
				continue; /* e.g. a TABLENAME.ASTERISK */
			}
			UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
			UNPACK_SQL_STATEMENT(column, column_alias->column, column);
			if ((table_alias->unique_id != unique_id) || !IS_KEY_COLUMN(column)
			    || (lp_get_key_num(column) > max_key_num)) {
				continue; /* Not a key column of this table that the caller asks about */
			}
			if ((NULL != key_unique_id_array)
			    && !can_fix_key_of_table(cond->v.lp_default.operand[1 - i], unique_id, key_unique_id_array)) {
				continue; /* The other operand fixes nothing of this table (see above) */
			}
			return TRUE;
		}
		return FALSE;
	default:
		return FALSE;
	}
}

/* Returns TRUE if the table "unique_id" is walked through a cross reference for an ORDER BY (YDBOcto#660) */
static boolean_t has_order_by_xref_walk(LogicalPlan *plan, int unique_id) {
	LogicalPlan *cur_key, *lp_key;

	for (cur_key = lp_get_keys(plan); NULL != cur_key;) {
		SqlKey *key;

		GET_LP(lp_key, cur_key, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		if ((key->unique_id == unique_id) && key->is_order_by_xref_walk) {
			return TRUE;
		}
		GET_LP_ALLOW_NULL(cur_key, cur_key, 1, LP_KEYS);
	}
	return FALSE;
}

/* Decide if a condition on a key column uses the column's cross reference instead of fixing the key in place
 *
 * Decides whether a condition on the primary key column "key" (a key that is not fixed yet) is better served by an AIM
 * subscript cross reference on that column (YDBOcto#660) than by fixing the key in place.
 *
 * Fixed in place, a key column after the first still runs inside the FOR loops of the key columns before it: a
 * "WHERE id = 5" on keys (acc, id) visits every "acc" and probes $DATA(^T(acc,5)) under each. The cross reference
 * holds every "id" with the "acc" values it occurs with, so the rows are found directly.
 *
 * A "LIKE 'prefix%'" on a string key column (with "is_like_prefix") needs the cross reference to walk the values that
 * start with the prefix, even for the first key column: the table's global holds a string key column's values in M
 * collation, where canonical numbers ('12', '120') come before all other strings ('12a'), so the values that share a
 * prefix are not one contiguous block there. The cross reference holds them as "#"_value, which keeps them together.
 *
 * The cross reference is used only when all of the following hold. Otherwise the key is fixed in place as before (or, for
 * a LIKE prefix, the LIKE is checked on every row).
 *   - The column is not the first key column, unless "is_like_prefix". The table's global already serves the first key
 *     column directly for any other condition.
 *   - The table is kept in a global (an AIM cross reference).
 *   - The table has no ITERATOR column. An ITERATOR decides which subscripts of the global are rows (it may skip some),
 *     but the cross reference indexes every stored subscript and gives the key columns before this one without calling
 *     the ITERATOR, so it would give rows that are not in the table.
 *   - The table has no cross reference yet, no key column before this one is fixed yet, and no condition of the WHERE
 *     or of an ON can fix a key column before this one (see "lp_is_key_column_compared()"; a comparison with a column
 *     of a table whose FOR loop comes later cannot, as that table's key is fixed to this one). A key column after
 *     this one may already be fixed, so the order of the conditions does not change the plan.
 *   - The condition gives the column one value ("=", "IS"), or the query has no LIMIT, or the FOR loops of the table's
 *     key columns do not give the ORDER BY order (see "lp_is_order_by_with_limit_given_by_key_loops()"). With a LIMIT,
 *     a walk of the cross reference through several values (a range, IN or LIKE prefix) would need a sort of every row
 *     it reads, where the FOR loops need no sort and stop at the LIMIT.
 */
static boolean_t use_key_column_xref(LogicalPlan *plan, SqlKey *key, SqlTableAlias *table_alias, LPActionType type,
				     int *key_unique_id_array) {
	SqlTable	   *table;
	SqlOptionalKeyword *source_keyword;
	LogicalPlan	   *cur_key, *lp_key, *where, *table_join;
	int		    key_num;
	boolean_t	    is_like_prefix, is_one_value;

	is_like_prefix = ((LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY == type) || (LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX == type)
			  || (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX == type));
	assert(!key->is_cross_reference_key && (NULL != key->column) && IS_KEY_COLUMN(key->column));
	assert(!is_like_prefix || IS_COLUMN_STRING_TYPE(key->column));
	key_num = lp_get_key_num(key->column);
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	if ((0 == key_num) && !is_like_prefix && !IS_XREF_VALUE_TRANSFORMED(table, key->column)) {
		/* The first key column: the table's global serves it. Not if its cross reference holds converted values:
		 * the key cannot be fixed in place then ("is_transformed_key_column()").
		 */
		return FALSE;
	}
	UNPACK_SQL_STATEMENT(source_keyword, table->source, keyword);
	if ('^' != source_keyword->v->v.value->v.string_literal[0]) {
		return FALSE; /* A table kept in a local variable has no AIM cross reference */
	}
	if (table->has_iterator) {
		return FALSE; /* An ITERATOR decides which subscripts are rows; the cross reference does not call it */
	}
	is_one_value = ((LP_BOOLEAN_EQUALS == type) || (LP_BOOLEAN_IS == type)); /* Not a range, an IN or a LIKE prefix */
	if (!is_one_value && lp_is_order_by_with_limit_given_by_key_loops(plan, table_alias, key_num)) {
		return FALSE; /* With a LIMIT, the FOR loops give the ORDER BY order: no sort, and they stop at the LIMIT */
	}
	/* A table gets at most one cross reference. A key column before this one that is already fixed makes the loops
	 * before this key run once, so fixing this key in place is then the cheaper plan. A key column after this one that
	 * is already fixed is no obstacle: it is reached through the table's global after the cross reference. Whether
	 * such a key was processed before this one depends only on the order of the conditions.
	 */
	cur_key = lp_get_keys(plan);
	do {
		SqlKey *other_key;

		GET_LP(lp_key, cur_key, 0, LP_KEY);
		other_key = lp_key->v.lp_key.key;
		if (other_key->unique_id == table_alias->unique_id) {
			if (other_key->is_cross_reference_key || (NULL != other_key->cross_reference_output_key)) {
				return FALSE; /* The table already has its cross reference */
			}
			if ((LP_KEY_FIX == other_key->type) && (NULL != other_key->column) && IS_KEY_COLUMN(other_key->column)
			    && (lp_get_key_num(other_key->column) < key_num)) {
				return FALSE; /* An earlier key column is fixed: the loops before this key run once */
			}
		}
		GET_LP_ALLOW_NULL(cur_key, cur_key, 1, LP_KEYS);
	} while (NULL != cur_key);
	where = lp_get_select_where(plan);
	if (lp_is_key_column_compared(where->v.lp_default.operand[0], table_alias->unique_id, key_num - 1, key_unique_id_array)) {
		return FALSE; /* An earlier key column is fixed by another condition: the loops before this key run once */
	}
	table_join = lp_get_table_join(plan);
	do {
		/* The ON condition of a JOIN that follows an OUTER JOIN is kept apart from the WHERE */
		if (lp_is_key_column_compared(table_join->extra_detail.lp_table_join.join_on_condition, table_alias->unique_id,
					      key_num - 1, key_unique_id_array)) {
			return FALSE; /* An earlier key column is fixed by an ON condition: as above */
		}
		GET_LP_ALLOW_NULL(table_join, table_join, 1, LP_TABLE_JOIN);
	} while (NULL != table_join);
	return TRUE;
}

/* Return the comparison of a condition as it applies to the key it fixes
 *
 * The key is the left operand of the comparison "type". If the operands were swapped to make it so ("operands_swapped"), a
 * range comparison is reversed: "5 > id" is "id < 5". Any other comparison ("=", IN etc.) is returned as is.
 */
static LPActionType get_key_fix_type(LPActionType type, boolean_t operands_swapped) {
	if (!operands_swapped) {
		return type;
	}
	switch (type) {
	case LP_BOOLEAN_LESS_THAN:
		return LP_BOOLEAN_GREATER_THAN;
	case LP_BOOLEAN_GREATER_THAN:
		return LP_BOOLEAN_LESS_THAN;
	case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
		return LP_BOOLEAN_GREATER_THAN_OR_EQUALS;
	case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
		return LP_BOOLEAN_LESS_THAN_OR_EQUALS;
	default:
		return type;
	}
}

/* Test if a range condition bounds a key from the side its range is still open
 *
 * Returns TRUE if an earlier condition fixed "key" to a range ("<", "<=", ">" or ">=") of the column "column" of the table
 * "unique_id", and the comparison "type" on that column bounds it from the other side (YDBOcto#1020). E.g. "id >= 3" fixed
 * the key of "id", and "id <= 5" is the other bound: the walk of the key then starts at 3 and ends at 5, where it would
 * otherwise go on to the end of the table's global or cross reference.
 */
static boolean_t is_other_bound_of_range(SqlKey *key, LPActionType type, SqlColumn *column, int unique_id) {
	if ((LP_KEY_FIX != key->type) || (NULL != key->fixed_to_value2)) {
		return FALSE; /* The key is not fixed yet, or its range has both bounds already */
	}
	if ((key->column != column) || (key->unique_id != unique_id)) {
		/* Another key of the table. "lp_get_key()" returns a fixed key of the table for a column that has no key. */
		return FALSE;
	}
	if (!IS_KEY_FIX_RANGE_TYPE(key->fixed_to_value_type) || !IS_KEY_FIX_RANGE_TYPE(type)) {
		return FALSE; /* The key takes one value at a time ("=", IN or IS), or a LIKE prefix, or this is no range */
	}
	if (IS_KEY_FIX_LOWER_BOUND(key->fixed_to_value_type) == IS_KEY_FIX_LOWER_BOUND(type)) {
		/* A bound on the side that is already bounded (e.g. "id > 3 AND id > 5"): it is checked on each row */
		return FALSE;
	}
	return TRUE;
}

void lp_optimize_where_multi_equals_ands(LogicalPlan *plan, LogicalPlan *where, SqlTableAlias *right_table_alias,
					 boolean_t num_outer_joins) {
	int	    *key_unique_id_array; // keys_unique_id_ordering[unique_id] = index in the ordered list
	int	     max_unique_id;	  // 1 more than maximum possible table_id/unique_id
	int	     i;
	LogicalPlan *cur, *lp_key;
	SqlKey	    *key;

	max_unique_id = config->plan_id; /* similar logic exists in "lp_optimize_cross_join()" */
	key_unique_id_array = octo_cmalloc(memory_chunks, sizeof(int) * max_unique_id); /* guarantees 0-initialized memory */
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
		GET_LP_ALLOW_NULL(cur, cur, 1, LP_KEYS);
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
	LogicalPlan    *cur, *left, *right, *t;
	LogicalPlan    *lp_key;
	SqlColumnAlias *column_alias;
	SqlTableAlias  *table_alias;
	SqlKey	       *key;
	int		left_id, right_id;
	LogicalPlan    *new_left, *new_right, *list;
	LPActionType	type;
	SqlTableAlias  *right_table_alias;
	boolean_t	is_null;
	boolean_t	is_transformed_key_condition; /* A condition on a key column that only its cross reference can serve */

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
	/* If the plan is a BOOLEAN IS/IS NOT clause, then we need to determine whether the second operand (i.e., `right`) is NULL,
	 * since this case requires special handling. So, check whether the right operand is a SqlValue of type `NUL_VALUE` and
	 * store in a variable for repeated reference below.
	 */
	if ((LP_BOOLEAN_IS == type) || (LP_BOOLEAN_IS_NOT == type)) {
		assert(LP_VALUE == right->type);
		is_null = IS_NUL_VALUE(right->v.lp_value.value->type);
	} else {
		is_null = FALSE;
	}
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
	case LP_BOOLEAN_IS_NOT:
		if (!is_null) {
			// We cannot optimize `IS NOT TRUE` or `IS NOT FALSE`, so skip optimization and return here
			return where;
		}
		/* Fall through */
	case LP_BOOLEAN_IS:
	case LP_BOOLEAN_EQUALS:
	case LP_BOOLEAN_LESS_THAN:
	case LP_BOOLEAN_GREATER_THAN:
	case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
		/* A key column whose cross reference holds converted values ("is_transformed_key_column()") can only be served
		 * by that cross reference ("is_transformed_key_condition" below). The order of the tables is chosen without such
		 * a condition, as before ("lp_optimize_cross_join()", the caller when "key_unique_id_array" is NULL). IS NULL is
		 * never true for a key column, so it is checked on each row: a cross reference has nothing to find for it.
		 * IS NOT NULL is always true for it, and is removed below as for any key column.
		 */
		if (((LP_COLUMN_ALIAS == left->type) && is_transformed_key_column(left))
		    || ((LP_COLUMN_ALIAS == right->type) && is_transformed_key_column(right))) {
			if ((NULL == key_unique_id_array) || (LP_BOOLEAN_IS == type)) {
				return where;
			}
		}
		break;
	case LP_BOOLEAN_IN:
		/* Check if left side of IN is a LP_COLUMN_ALIAS. If not, we cannot do key fixing. */
		if (LP_COLUMN_ALIAS != left->type) {
			return where;
			break;
		}
		if (is_transformed_key_column(left) && (NULL == key_unique_id_array)) {
			return where; /* As for "=" above: the order of the tables is chosen without this condition */
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
			GET_LP_ALLOW_NULL(list, list, 1, LP_COLUMN_LIST);
		} while (NULL != list);
		break;
	case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY:
	case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX:
	case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX:
	case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK:
		/* A LIKE or ILIKE whose pattern starts with a literal prefix (YDBOcto#1158). The cross reference of the
		 * column can be walked over that prefix only if the column is on the left and the pattern literal is on the
		 * right. LIKE is not symmetric, so the operands are never swapped (unlike "=" or "<" etc.).
		 */
		if ((LP_COLUMN_ALIAS != left->type) || (LP_VALUE != right->type)) {
			return where;
		}
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
	table_alias = NULL;  /* needed to avoid incorrect [-Wmaybe-uninitialized] C compiler warning */
	column_alias = NULL; /* needed to avoid incorrect [-Wmaybe-uninitialized] C compiler warning */
	switch (left->type) {
	case LP_NEGATIVE:
	case LP_FORCE_NUM:
		/* A sign before a value or a parameter (e.g. "-1", which the parser makes a unary minus of the literal 1,
		 * YDBOcto#1020) is a value too: the plan computes it once, before the FOR loop of the key. A sign before
		 * anything else (e.g. "-v") is not.
		 */
		if (LP_VALUE != left->v.lp_default.operand[0]->type) {
			return where; /* A sign before a column or an expression: no key to fix */
		}
		/* fall through */
	case LP_VALUE:
		left_id = 0;
		break;
	case LP_COLUMN_ALIAS:
		column_alias = left->v.lp_column_alias.column_alias;
		if (is_stmt_table_asterisk(column_alias->column)) {
			return where;
		} else {
			UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
			left_id = table_alias->unique_id;
			assert(0 < left_id);
		}
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

	switch (right->type) {
	case LP_NEGATIVE:
	case LP_FORCE_NUM:
		/* A sign before a value or a parameter (e.g. "-1", which the parser makes a unary minus of the literal 1,
		 * YDBOcto#1020) is a value too: the plan computes it once, before the FOR loop of the key. A sign before
		 * anything else (e.g. "-v") is not.
		 */
		if (LP_VALUE != right->v.lp_default.operand[0]->type) {
			return where; /* A sign before a column or an expression: no key to fix */
		}
		/* fall through */
	case LP_VALUE:
		right_id = 0;
		break;
	case LP_COLUMN_ALIAS:
		column_alias = right->v.lp_column_alias.column_alias;
		if (is_stmt_table_asterisk(column_alias->column)) {
			return where;
		} else {
			UNPACK_SQL_STATEMENT(table_alias, right->v.lp_column_alias.column_alias->table_alias_stmt, table_alias);
			right_id = table_alias->unique_id;
			assert(0 < right_id);
		}
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
	boolean_t operands_swapped;
	operands_swapped = FALSE;
	/* From here on "num_outer_joins" is no longer overloaded and a non-zero value truly implies OUTER JOIN usage.
	 * Hence a few checks for OUTER JOIN usage are done after this point.
	 */
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
		// Fix the column of the table whose FOR loop comes later; a column of a parent query (key_unique_id_array[] == 0)
		// is never fixed (see "is_column_fixed_to_other_table()", and the comment in the function
		// "lp_optimize_where_multi_equals_ands()" for more details on this). If that is the right column, swap.
		if (!is_column_fixed_to_other_table(key_unique_id_array, left_id, right_id)) {
			t = left;
			left = right;
			right = t;
			operands_swapped = TRUE;
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
			operands_swapped = TRUE;
		} else if (left_id) {
			// The right is a value, left a column. Check if left comes in from parent query.
			// In that case, we cannot fix it. Else it is already in the right order so no swap needed
			// like was the case in the previous "if" block.
			if (0 == key_unique_id_array[left_id]) {
				// The left column corresponds to a unique_id coming in from the parent query.
				// In that case we cannot fix it in the sub-query. Return.
				return where;
			}
			/* Now that we know the left side is a column reference, check for one of the below optimizations.
			 * 1) If it is an IS NOT NULL operation AND if the column corresponds to a primary key column.
			 *    In that case, we are guaranteed the primary key column can never take on a NULL value.
			 *    That is, the IS NOT NULL condition would always evaluate to TRUE.
			 *    So no need to check it for each row. Therefore remove the `LP_BOOLEAN_IS_NOT`
			 *    from the ON/WHERE clause.
			 * 2) If it is an IS NULL operation, we can fix the key.
			 */
			if (((LP_BOOLEAN_IS_NOT == type) || (LP_BOOLEAN_IS == type))) {
				/* Note: "column_alias" and "table_alias" would have been initialized in the "switch(left->type)"
				 * code block above if we reach here. But the C compiler incorrectly issues a
				 * "[-Wmaybe-uninitialized]" warning here and therefore we initialize these to "NULL"
				 * before the "switch(left->type)" code block above.
				 */
				if (create_table_STATEMENT != table_alias->table->type) {
					/* It is an on-the-fly table constructued using the VALUES clause. Such a table does
					 * not have PRIMARY KEY columns so no IS NOT NULL or IS NULL optimizations possible.
					 */
					assert(table_value_STATEMENT == table_alias->table->type);
					return where;
				}
				assert(NULL != column_alias);
				if (column_alias->column->type == column_STATEMENT) {
					SqlColumn *column;
					UNPACK_SQL_STATEMENT(column, column_alias->column, column);
					if (LP_BOOLEAN_IS_NOT == type) {
						assert(is_null);
						if (num_outer_joins) {
							/* OUTER JOINs can cause NULL values even for primary keys and so we
							 * cannot optimize IS NOT NULL in that case. A similar check for the
							 * IS NULL case will be done a little later in this function.
							 */
							return where;
						}
						if (IS_KEY_COLUMN(column)) {
							/* IS NOT NULL on a key column. Can be safely removed from the
							 * ON/WHERE clause now that we know .
							 */
							if (LP_WHERE == where->type) {
								where->v.lp_default.operand[0] = NULL;
							}
							return NULL;
						} else {
							/* IS NOT NULL on a non-key column. Cannot be fixed to a constant value.
							 * Return ON/WHERE clause with no change (i.e. no optimization possible).
							 */
							return where;
						}
					}
					/* else : LP_BOOLEAN_IS on a LP_COLUMN_ALIAS. Fall through for key-fixing. */
				} else {
					/* IS NOT NULL or IS NULL on a TABLENAME.ASTERISK type of column. No optimizations possible.
					 * Return ON/WHERE clause with no change (i.e. no optimization possible).
					 */
					assert(is_stmt_table_asterisk(column_alias->column));
					return where;
				}
			}
		} else {
			// Both left and right are values.
			assert((LP_VALUE == left->type) || (LP_NEGATIVE == left->type) || (LP_FORCE_NUM == left->type));
			assert((LP_VALUE == right->type) || (LP_NEGATIVE == right->type) || (LP_FORCE_NUM == right->type));
			/* This is one of the following cases.
			 *	WHERE 5=5		-> LP_BOOLEAN_EQUALS
			 *	WHERE 5<3		-> LP_BOOLEAN_LESS_THAN
			 *	WHERE 5>3		-> LP_BOOLEAN_GREATER_THAN
			 *	WHERE 5<=3		-> LP_BOOLEAN_LESS_THAN_OR_EQUALS
			 *	WHERE 5>=3		-> LP_BOOLEAN_GREATER_THAN_OR_EQUALS
			 *	WHERE 5 IS NULL		-> LP_BOOLEAN_IS
			 *	WHERE 5 IS NOT NULL	-> LP_BOOLEAN_IS_NOT
			 * Nothing we can do here in terms of optimizing.
			 */
			return where;
		}
	}
	/* The key to fix is now "left" (the operands were swapped above if needed: a column of a later table is fixed to a
	 * value or to a column of an earlier table). If it is a key column whose cross reference holds converted values, only
	 * its cross reference can serve the condition: the value it is fixed to, a constant or the column of the other table,
	 * is in the converted form, as the cross reference is (YDBOcto#660).
	 */
	is_transformed_key_condition = ((LP_COLUMN_ALIAS == left->type) && is_transformed_key_column(left));
	assert((LP_BOOLEAN_EQUALS == type) || (LP_BOOLEAN_LESS_THAN == type) || (LP_BOOLEAN_GREATER_THAN == type)
	       || (LP_BOOLEAN_LESS_THAN_OR_EQUALS == type) || (LP_BOOLEAN_GREATER_THAN_OR_EQUALS == type) || (LP_BOOLEAN_IN == type)
	       || (LP_BOOLEAN_IS == type) || (LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY == type)
	       || (LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX == type) || (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX == type)
	       || (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK == type));
	assert(!operands_swapped
	       || ((LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY != type) && (LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX != type)
		   && (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX != type)
		   && (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK != type)));
	right_table_alias = (SqlTableAlias *)ptr;
	if (NULL != right_table_alias) {
		/* This is part of a JOIN ON clause (`right_table_alias` variable non-NULL). In this case, we cannot fix the key
		 * if it corresponds to a column reference i.e. LP_COLUMN_ALIAS (see YDBOcto#426 for examples that fail) from a
		 * previous table in the join list. But if the key being fixed belongs to the RIGHT side table of this JOIN then
		 * it is safe to fix. Check that. Note that the key being fixed corresponds to the variable `left` hence the
		 * check for `left->type` below.
		 */
		if (LP_COLUMN_ALIAS == left->type) {
			SqlColumnAlias *column_alias;
			SqlTableAlias  *column_table_alias;

			column_alias = left->v.lp_column_alias.column_alias;
			UNPACK_SQL_STATEMENT(column_table_alias, column_alias->table_alias_stmt, table_alias);
			if (column_table_alias != right_table_alias) {
				/* The column reference being fixed does not belong to the RIGHT side table of OUTER JOIN.
				 * Cannot fix this. Return unfixed plan as is.
				 */
				return where;
			}
		}
	} else {
		/* This is part of a WHERE clause. Check if this is an IS NULL check and OUTER JOINs are present. If so,
		 * we cannot fix the key since fixing the value to NULL can confuse with NULLs created by the OUTER JOINs.
		 */
		if (is_null && num_outer_joins) {
			return where;
		}
	}
	// Get the table alias and column for left
	column_alias = left->v.lp_column_alias.column_alias;
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
	// TODO: how do we handle triggers on generated tables?
	if (create_table_STATEMENT != table_alias->table->type) {
		/* There is no cross reference possible for an on-the-fly table constructed using the VALUES clause */
		assert(table_value_STATEMENT == table_alias->table->type);
		return where;
	}

	SqlColumn *column;
	assert(column_STATEMENT == column_alias->column->type);
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	switch (type) {
	case LP_BOOLEAN_LESS_THAN:
	case LP_BOOLEAN_GREATER_THAN:
	case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
	case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
		if (NULL != get_keyword(column, OPTIONAL_END)) {
			/* This is a column which has END keyword specified. It requires more work to support this with the
			 * key-fixing optimization than is considered worth the effort at this point so we return in this case.
			 * See OPTIONAL_END usage in tmpl_tablejoin.ctemplate and surrounding comments for more details.
			 */
			return where;
		}
		break;
	case LP_BOOLEAN_IS:
	case LP_BOOLEAN_IN:
	case LP_BOOLEAN_EQUALS:
		break;
	case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY:
	case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX:
	case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX:
	case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK:
		/* No END check needed. This only walks the AIM cross reference of a non-key column (a key column is rejected
		 * further below), and END only ends the FOR loop of a key column over the table's own global.
		 */
		break;
	default:
		assert(FALSE);
		break;
	}
	boolean_t is_like_prefix;
	is_like_prefix
	    = ((LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY == type) || (LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX == type)
	       || (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX == type) || (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK == type));
	key = lp_get_key(plan, left);
	/* A condition on a primary key column after the first one, or a LIKE prefix on a string primary key column, is served
	 * by a cross reference on that column when "use_key_column_xref()" says so (YDBOcto#660). The new cross reference key
	 * replaces the key; it is fixed below as the cross reference key of a non-key column is.
	 */
	if ((NULL != key) && !key->is_cross_reference_key && (NULL != key->column) && IS_KEY_COLUMN(key->column)
	    && (LP_KEY_ADVANCE == key->type) && (NULL == key->cross_reference_output_key)
	    && (!is_like_prefix || IS_COLUMN_STRING_TYPE(key->column))
	    && use_key_column_xref(plan, key, table_alias, type, key_unique_id_array)) {
		SqlTable *table;
		SqlKey	 *xref_key;

		UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
		xref_key = lp_insert_xref_key(plan, table, column_alias, table_alias);
		if (NULL != xref_key) {
			key = xref_key;
		}
	}
	if (is_transformed_key_condition && ((NULL == key) || !key->is_cross_reference_key)) {
		/* The condition is on a key column that only its cross reference can serve, and the cross reference is not
		 * used (e.g. the table already has one, or a key column before it has a condition). The key cannot be fixed
		 * in place, so the condition is checked on each row.
		 */
		return where;
	}
	if (is_like_prefix && (NULL != key) && (!key->is_cross_reference_key || ('^' != key->xref_prefix[0]))) {
		/* LIKE with a prefix on a key column that has no cross reference (see above). The rows are walked in the order
		 * of the table's own global, where values that are canonical numbers (e.g. '12', '120') sort before all other
		 * strings (e.g. '12a'). So the values sharing a prefix do not form one contiguous block and a prefix walk would
		 * miss rows. The same is true of a cross reference kept in a local variable (see the "is_like_prefix" check
		 * below).
		 */
		return where;
	}
	// If the left isn't a key, generate a cross reference
	if (NULL == key) {
		SqlTable *table;

		if (has_order_by_xref_walk(plan, table_alias->unique_id)) {
			/* With a LIMIT, an ORDER BY already walks a cross reference of this table (YDBOcto#660, see
			 * "lp_optimize_order_by_with_limit()"). A table has one cross reference: the condition is checked on
			 * each row of the walk.
			 */
			return where;
		}
		UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
		// Tables with iterators cannot be cross-referenced
		if (TRUE == table->has_iterator) {
			return where;
		}
		if (is_like_prefix) {
			SqlOptionalKeyword *like_source_keyword;

			/* Same choice of source as the "key->xref_prefix" computation below */
			like_source_keyword = get_keyword(column, OPTIONAL_GLOBAL);
			if (NULL == like_source_keyword) {
				UNPACK_SQL_STATEMENT(like_source_keyword, table->source, keyword);
			}
			if ('^' != like_source_keyword->v->v.value->v.string_literal[0]) {
				/* The column lives in a local variable, so its cross reference would too. That cross reference
				 * stores a string column value as is, so canonical numbers sort before all other strings there
				 * as well (see the key column case above). Only an AIM cross reference (of a column in a global)
				 * stores a string column value as "#"_value, which keeps all values that share a prefix together.
				 */
				return where;
			}
		}
		key = lp_insert_xref_key(plan, table, column_alias, table_alias);
		if (NULL == key) {
			return where;
		}
	}
	/* The comparison as it applies to the key: "5 > id" bounds "id" as "id < 5" does */
	LPActionType fixed_type;
	fixed_type = get_key_fix_type(type, operands_swapped);

	/* A key that is already fixed to one bound of a range can also take the other bound (YDBOcto#1020) */
	boolean_t is_second_bound;
	is_second_bound = is_other_bound_of_range(key, fixed_type, column, table_alias->unique_id);

	/* See if we can fix the key (`left` variable) to `right` variable. Note that we can't fix keys that are already fixed
	 * (other than with the second bound of a range), or keys that are part of a cross reference iteration. Hence the `if`
	 * check below.
	 */
	if (((LP_KEY_ADVANCE == key->type) && (NULL == key->cross_reference_output_key)) || is_second_bound) {
		boolean_t remove_lp_boolean_equals;

		if (is_null) {
			SqlValue *value;

			/* Fix the IS NULL value to be the empty string (so that value gets used in the xref) */
			MALLOC_LP_2ARGS(right, LP_VALUE);
			OCTO_CMALLOC_STRUCT(value, SqlValue);
			value->type = IS_NULL_LITERAL; /* Special type to indicate this is key fixing to NULL for "IS NULL".
							* Key fixing to NULL for "= NULL" will use NUL_VALUE type.
							*/
			value->v.string_literal = "";
			right->v.lp_value.value = value;
			assert(IS_NULL_FIXED_VALUE(right));
		}
		if (is_second_bound) {
			/* The key's walk ends at this bound, instead of at the end of the global or cross reference */
			key->fixed_to_value2 = right;
			key->fixed_to_value2_type = fixed_type;
		} else {
			key->fixed_to_value = right;
			key->type = LP_KEY_FIX;
			key->fixed_to_value_type = fixed_type;
		}
		/* Anytime a "LP_BOOLEAN_EQUALS etc." is mentioned below, it implies actually any one of the following
		 *	LP_BOOLEAN_EQUALS
		 *	LP_BOOLEAN_LESS_THAN
		 *	LP_BOOLEAN_GREATER_THAN
		 *	LP_BOOLEAN_LESS_THAN_OR_EQUALS
		 *	LP_BOOLEAN_GREATER_THAN_OR_EQUALS
		 */
		/* Now that a key has been fixed to a constant (or another key), see if we can eliminate this LP_BOOLEAN_EQUALS etc.
		 * from the WHERE or ON clause expression as it will otherwise result in a redundant IF check in the generated
		 * M code. We cannot do this removal in case this is the WHERE clause and OUTER JOINs exist (it is okay to do
		 * this if it is the ON clause and OUTER JOINs exist) as we need == check for this WHERE clause to be generated
		 * in the M code for correctness of query results. If we can remove it safely, there is still a subtlety involved
		 * with keys from parent queries which is handled further below.
		 */
		remove_lp_boolean_equals = ((NULL != right_table_alias) || (0 == num_outer_joins));
		if ((LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX == type) || (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX == type)
		    || (LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK == type)) {
			/* The pattern has more after its prefix (e.g. 'PAT%ENT', or the ILIKE '12A%' whose prefix is '12'). The
			 * cross reference walk only guarantees the prefix, so each row it finds still needs the full LIKE or
			 * ILIKE check. Keep it in the WHERE/ON clause. For LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK (e.g.
			 * '12A%'), that check compares the start of the value with the literal, both in lower case.
			 * With LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY (e.g. 'PAT%'), the walk finds exactly the rows the
			 * LIKE matches, so the LIKE can be removed like an "=".
			 */
			remove_lp_boolean_equals = FALSE;
		}
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
			/* Do not remove this LP_BOOLEAN_EQUALS etc. from the WHERE clause due to the presence of OUTER JOINs */
		}
	}
	return where;
}
