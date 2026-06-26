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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/* Decides whether and how the cross reference on "column" can be used to drive the table's key iteration.
 *
 * Inputs:
 *   "column"      - the non-key column the WHERE filter is on (the cross reference is built on its global).
 *   "table"       - the table that "column" belongs to.
 *   "key_columns" - the table's primary-key columns, indexed by primary-key number (0 through "max_key"),
 *                   as returned by "get_key_columns()".
 *   "max_key"     - the highest primary-key number; the table has "max_key" + 1 primary-key columns.
 * Output:
 *   "advance_key_columns" - a caller-allocated array of at least MAX_KEY_COUNT entries. On success (return
 *                   value >= 0) it is filled with the table's primary-key columns in the order they should
 *                   be iterated (see below). Untouched when -1 is returned.
 *
 * On success, fills "advance_key_columns" with all of the table's primary-key columns (there are
 * "max_key" + 1 of them) in the order they should be iterated, and returns how many of those leading
 * columns are advanced from the cross reference. Calling that returned count N: advance_key_columns[0]
 * through advance_key_columns[N - 1] are advanced from the cross reference (AIM) global, and
 * advance_key_columns[N] through advance_key_columns[max_key] are advanced from the table's own global.
 * Returns -1 if the cross reference cannot be used at all (the caller then declines it and falls back to a
 * full scan).
 *
 * The cross reference (AIM or raw xref) indexes the global that "column" itself is mapped to, and stores one
 * subscript per "keys(...)" in that global, in the order the global lists them. The remaining primary-key
 * columns (if any) live on a deeper node of the table's own global. Exactly one shape can use it:
 *
 *   The column's global lists a LEADING PREFIX of the primary key, IN PRIMARY-KEY ORDER. The listed keys are
 *   advanced from the cross reference; the deeper primary-key columns are advanced from the table's own
 *   global. (A value stored once on a parent node shared by its child rows is this shape.) A global listing
 *   every primary-key column in primary-key order is the full-length case of this and needs nothing extra.
 *
 * Every other shape is declined. That covers a subset which skips a primary-key column, and also all the
 * primary-key columns present but in an order other than the primary key's.
 *
 * The reordered case used to be accepted (YDBOcto#1124), by advancing the keys in the column's global order
 * so each cross-reference subscript bound to the right column. That worked while the cross reference was
 * built from the column's own global alone. YDBAIM#96 additionally gives it a record anchor on the TABLE's
 * global, so that a row whose value node is absent can be indexed as SQL NULL; AIM binds the anchor's key
 * subscripts to the value spec's POSITIONALLY, and cannot do otherwise, because it is handed ":" marks and
 * never knows which column each one stands for.
 *
 * A column with no GLOBAL mapping (or a bare, unsubscripted global, which Octo auto-fills with all primary-key
 * subscripts) is a piece of the table's row at full primary-key depth, so all primary-key columns are covered.
 * (YDBOcto#1124)
 */
static int get_xref_advance_keys(SqlColumn *column, SqlTable *table, SqlColumn **key_columns, int max_key,
				 SqlColumn **advance_key_columns) {
	SqlOptionalKeyword *global_keyword;
	SqlValue	   *value;
	SqlColumn	   *global_key_columns[MAX_KEY_COUNT];
	int		    num_keys, cur_key;

	/* Get the ordered list of primary-key columns the column's own global references ("global_key_columns"
	 * / "num_keys"). A column with no GLOBAL keyword, or a bare unsubscripted global, maps to the table's row
	 * at full primary-key depth; represent that as a global that lists every primary-key column in
	 * primary-key order, so it flows through the common logic below as a (full) leading prefix.
	 */
	global_keyword = get_keyword(column, OPTIONAL_GLOBAL);
	if (NULL != global_keyword) {
		UNPACK_SQL_STATEMENT(value, global_keyword->v, value);
		num_keys = get_keys_in_global(value->v.string_literal, table, global_key_columns);
	} else {
		num_keys = 0;
	}
	if (0 == num_keys) {
		for (cur_key = 0; cur_key <= max_key; cur_key++) {
			global_key_columns[cur_key] = key_columns[cur_key];
		}
		num_keys = max_key + 1;
	} else if ((max_key + 1) < num_keys) {
		/* More keys than the primary key has. Not expected for a validated mapping; decline defensively. */
		return -1;
	}

	/* The cross reference can drive the iteration in exactly one shape (see the function header): a leading
	 * prefix of the primary key, in primary-key order. Anything else is declined.
	 */
	for (cur_key = 0; cur_key < num_keys; cur_key++) {
		if (global_key_columns[cur_key] != key_columns[cur_key]) {
			return -1;
		}
	}

	/* The first "num_keys" primary-key columns are advanced from the cross reference. Any remaining
	 * primary-key columns are advanced from the table's own global. Both runs are in primary-key order,
	 * which the loop above has just established for the first of them.
	 */
	for (cur_key = 0; cur_key < num_keys; cur_key++) {
		advance_key_columns[cur_key] = global_key_columns[cur_key];
	}
	for (cur_key = num_keys; cur_key <= max_key; cur_key++) {
		advance_key_columns[cur_key] = key_columns[cur_key];
	}
	return num_keys;
}

/**
 * Returns the keys corresponding to the cross reference for column in table, and updates
 * the LP_TABLE_JOIN of plan to include the plan which needs to be execute to generate the cross
 * reference
 */
LogicalPlan *lp_generate_xref_keys(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias) {
	LogicalPlan *root, *keys, *table_join, *lp_table_alias, *lp_output_key;
	int	     cur_key, max_key, unique_id, num_xref_keys;
	SqlColumn   *key_columns[MAX_KEY_COUNT], *advance_key_columns[MAX_KEY_COUNT], *column;
	SqlKey	    *output_key;

	unique_id = table_alias->unique_id;
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	// Check if column is a computed column. If so, we cannot create a cross-reference on this.
	if (NULL != get_keyword_from_keywords(column->keywords->v.keyword, OPTIONAL_EXTRACT))
		return NULL;

	/* Determine which primary-key columns the cross reference can advance, and in what order to iterate all of
	 * the table's primary-key columns ("advance_key_columns"). If the column's global is mapped in a way the
	 * cross-reference iteration cannot correctly walk, decline the cross reference here (before mutating the
	 * plan below) so the caller falls back to a full scan that filters correctly. Primary-key columns not
	 * covered by the cross reference (if any) are advanced from the table's own global rather than the cross
	 * reference (signalled by the "NULL" output key passed to "lp_alloc_key" below). (YDBOcto#1124)
	 */
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
	/* "get_xref_advance_keys" fills "advance_key_columns" only on success (return value >= 0), and only up to
	 * "max_key". Zero it first so it is fully initialized regardless (avoids a false-positive
	 * uninitialized-value warning from clang-analyzer-core.CallAndMessage at the "lp_alloc_key" call below).
	 */
	memset(advance_key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
	max_key = get_key_columns(table, key_columns);
	num_xref_keys = get_xref_advance_keys(column, table, key_columns, max_key, advance_key_columns);
	if (-1 == num_xref_keys) {
		return NULL;
	}

	// Scan through and replace the table
	table_join = lp_get_table_join(plan);
	do {
		assert(LP_TABLE_VALUE != table_join->v.lp_default.operand[0]->type); /* Caller should have ensured this */
		if (table_join->v.lp_default.operand[0]->type == LP_TABLE) {
			GET_LP(lp_table_alias, table_join, 0, LP_TABLE);
			if (lp_table_alias->v.lp_table.table_alias->unique_id == table_alias->unique_id)
				break;
		}
		if (NULL != table_join->v.lp_default.operand[1]) {
			GET_LP(table_join, table_join, 1, LP_TABLE_JOIN);
		} else {
			table_join = NULL;
		}
	} while (NULL != table_join);
	if (NULL == table_join)
		return NULL;
	table_join->v.lp_default.operand[0] = lp_generate_xref_plan(table, column, unique_id);
	if (NULL == table_join->v.lp_default.operand[0])
		return NULL;
	lp_output_key = lp_get_output_key(table_join->v.lp_default.operand[0]);
	output_key = lp_output_key->v.lp_key.key;

	MALLOC_LP(keys, root, LP_KEYS);
	for (cur_key = 0; cur_key <= max_key; cur_key++) {
		SqlKey *advance_output_key;

		advance_output_key = (cur_key < num_xref_keys) ? output_key : NULL;
		keys->v.lp_default.operand[0]
		    = lp_alloc_key(table, advance_key_columns[cur_key], unique_id, LP_KEY_ADVANCE, advance_output_key, FALSE, NULL);
		if (cur_key != max_key) {
			MALLOC_LP_2ARGS(keys->v.lp_default.operand[1], LP_KEYS);
			GET_LP(keys, keys, 1, LP_KEYS);
		}
	}
	return root;
}
