/****************************************************************
 *								*
 * Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"
#include "template_helpers.h"

/* Replaces the keys of the table "table_alias" in the LP_KEYS list of "plan" with a cross reference key on the non-key
 * column "column_alias" followed by the table's primary key columns (which are then advanced from the cross reference).
 * For a primary key column (YDBOcto#660) the cross reference key takes that column's place, and the other primary key
 * columns follow it (see "get_key_column_xref_advance_keys()").
 * It also replaces the table in the LP_TABLE_JOIN list with the plan that builds the cross reference
 * (see "lp_generate_xref_keys()").
 *
 * Returns the new cross reference key (of type LP_KEY_ADVANCE), or NULL if the column cannot be cross referenced, in
 * which case "plan" is left untouched.
 *
 * Callers are "lp_optimize_where_multi_equals_ands_helper()" (a WHERE/ON condition on the column, which then fixes the
 * returned key) and "lp_optimize_order_by()" (an ORDER BY on the column, which walks the returned key, YDBOcto#1160).
 */
SqlKey *lp_insert_xref_key(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias) {
	LogicalPlan *first_key, *before_first_key, *last_key, *before_last_key, *xref_keys, *generated_xref_keys, *t;
	SqlKey	    *key;

	generated_xref_keys = lp_generate_xref_keys(plan, table, column_alias, table_alias);
	if (NULL == generated_xref_keys) {
		/* The column cannot be cross referenced (e.g. an EXTRACT column, or a global mapping the cross reference
		 * cannot walk). "lp_generate_xref_keys()" has not touched the plan in that case.
		 */
		return NULL;
	}

	// Remove all keys for the table alias
	before_first_key = lp_get_criteria(plan);
	first_key = lp_get_keys(plan);
	do {
		GET_LP(t, first_key, 0, LP_KEY);
		if (t->v.lp_key.key->unique_id == table_alias->unique_id) {
			break;
		}
		/* Both callers know that the table has keys in LP_KEYS, so the end of LP_KEYS is never reached before them */
		assert(NULL != first_key->v.lp_default.operand[1]);
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
	/* Insert the generated keys in place of the table's keys: the cross reference key of the column first (YDBOcto#660
	 * for a primary key column), then the keys advanced from it.
	 */
	key = generated_xref_keys->v.lp_default.operand[0]->v.lp_key.key;
	assert(key->is_cross_reference_key);
	if (LP_CRITERIA == before_first_key->type) {
		before_first_key->v.lp_default.operand[0] = generated_xref_keys;
	} else {
		before_first_key->v.lp_default.operand[1] = generated_xref_keys;
	}
	/* Note: It is possible the primary key for the table comprises of multiple columns in which case
	 * we need to scan down the xref_keys list (through v.operand[1]) until we hit the end, i.e. we
	 * cannot assume there is only one column comprising the primary key. Hence the for loop below.
	 */
	for (xref_keys = generated_xref_keys; NULL != xref_keys->v.lp_default.operand[1];
	     xref_keys = xref_keys->v.lp_default.operand[1])
		;
	if (NULL != before_last_key->v.lp_default.operand[1]) {
		xref_keys->v.lp_default.operand[1] = before_last_key->v.lp_default.operand[1];
	}
	return key;
}
