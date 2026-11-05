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
#include "template_helpers.h"

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
 * A column with no GLOBAL mapping is a piece of the table's row at full primary-key depth, so all primary-key
 * columns are covered (YDBOcto#1124). A column whose own GLOBAL has no key subscript has no cross reference
 * (YDBOcto#1191).
 */
static int get_xref_advance_keys(SqlColumn *column, SqlTable *table, SqlColumn **key_columns, int max_key,
				 SqlColumn **advance_key_columns) {
	int num_keys, cur_key;

	/* The number of primary-key columns the column's own global references, when they are a leading prefix of the
	 * primary key, in primary-key order: the one layout the cross reference can drive the iteration in (see the
	 * function header). Anything else is declined. A column with no GLOBAL keyword maps to the table's row at full
	 * primary-key depth: all of them.
	 */
	num_keys = get_column_global_key_prefix(column, table);
	if (-1 == num_keys) {
		return -1; /* The column's global lists the key columns in another order, or skips one */
	}
	if (0 == num_keys) {
		/* The column's global has no key subscript: one node holds its value for every row, and AIM cross references
		 * the subscripts of a node (it would stop with %YDBAIM-F-NOSUBS, YDBOcto#1191). "MIN"/"MAX" refuses it too
		 * ("is_octo617_optimization_possible()").
		 */
		return -1;
	}
	assert((max_key + 1) >= num_keys);

	/* The first "num_keys" primary-key columns are advanced from the cross reference. Any remaining
	 * primary-key columns are advanced from the table's own global. Both runs are in primary-key order.
	 */
	for (cur_key = 0; cur_key <= max_key; cur_key++) {
		advance_key_columns[cur_key] = key_columns[cur_key];
	}
	return num_keys;
}

/* Decide which key columns the cross reference of a key column advances, and in what order
 *
 * The same decision as "get_xref_advance_keys()", for a PRIMARY KEY column (YDBOcto#660).
 *
 * The cross reference of a key column is an AIM subscript cross reference (XREFSUB^%YDBAIM) on the TABLE's global. An
 * entry is (snum, value, <the other varying subscripts of the global, in global order>), and when the column is a string
 * (the "#"_value form) the column's own subscript is repeated among them. So:
 *   - the key columns before "column" are advanced from the cross reference, whose subscripts after the value start with
 *     them in either form;
 *   - the key columns after "column" are advanced from the table's own global, whose layout does not depend on the
 *     column type;
 *   - "column" itself is not among them: it is the cross reference key, which the caller puts in front of them.
 * E.g. keys (acc, id, line) with the cross reference on id: acc from the cross reference, line from the table's global.
 *
 * Fills "advance_key_columns" with the other "max_key" key columns in that order, and returns how many lead from the
 * cross reference, i.e. the KEY NUM of "column". The table's global lists every key column once, in KEY NUM order:
 * CREATE TABLE rejects any other GLOBAL ("validate_global_keyword()", ERR_GLOBAL_KEY_COLS_ORDER and
 * ERR_GLOBAL_MISSING_KEY_COLS). So the cross reference's subscripts after the value are the key columns in KEY NUM order.
 */
static int get_key_column_xref_advance_keys(SqlColumn *column, SqlTable *table, SqlColumn **key_columns, int max_key,
					    SqlColumn **advance_key_columns) {
	int cur_key, num_advance, column_key_num;

#ifndef NDEBUG
	SqlOptionalKeyword *source_keyword;
	SqlValue	   *value;
	SqlColumn	   *global_key_columns[MAX_KEY_COUNT];

	/* The table's global lists the key columns in KEY NUM order (see above) */
	UNPACK_SQL_STATEMENT(source_keyword, table->source, keyword);
	UNPACK_SQL_STATEMENT(value, source_keyword->v, value);
	assert((max_key + 1) == get_keys_in_global(value->v.string_literal, table, global_key_columns));
	for (cur_key = 0; cur_key <= max_key; cur_key++) {
		assert(global_key_columns[cur_key] == key_columns[cur_key]);
	}
#else
	UNUSED(table);
#endif

	column_key_num = -1;
	num_advance = 0;
	for (cur_key = 0; cur_key <= max_key; cur_key++) {
		if (key_columns[cur_key] == column) {
			column_key_num = cur_key;
		} else {
			advance_key_columns[num_advance++] = key_columns[cur_key];
		}
	}
	assert(0 <= column_key_num);
	assert(max_key == num_advance);
	return column_key_num;
}

/* Find the key of a key column of a table in a plan
 *
 * Returns the LP_KEY of "plan" for the key column "column" of the table "unique_id". The plan has a key for every key
 * column of every table it reads (the cross reference keys of this table are inserted only after this returns), so
 * there always is one.
 */
static LogicalPlan *find_table_key(LogicalPlan *plan, int unique_id, SqlColumn *column) {
	LogicalPlan *cur_key, *lp_key;
	SqlKey	    *key;

	cur_key = lp_get_keys(plan);
	while (NULL != cur_key) {
		GET_LP(lp_key, cur_key, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		if ((key->unique_id == unique_id) && (key->column == column) && !key->is_cross_reference_key) {
			return lp_key;
		}
		GET_LP_ALLOW_NULL(cur_key, cur_key, 1, LP_KEYS);
	}
	assert(FALSE); /* The key column has a key in the plan (see above) */
	return NULL;
}

/**
 * Returns the keys corresponding to the cross reference for column in table: the cross reference key of the column,
 * followed by the keys advanced from it (none for the cross reference of the only key column of a table, YDBOcto#660).
 * Updates the LP_TABLE_JOIN of plan to include the plan which needs to be execute to generate the cross reference.
 * Returns NULL, with the plan untouched, if the column cannot be cross referenced.
 */
LogicalPlan *lp_generate_xref_keys(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias) {
	LogicalPlan *root, *keys, *table_join, *lp_table_alias, *lp_output_key, *xref_key;
	int	     cur_key, max_key, unique_id, num_xref_keys, num_advance_keys;
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
	if (IS_KEY_COLUMN(column) && IS_XREF_VALUE_TRANSFORMED(table, column)) {
		/* A key column whose cross reference holds its values converted by a transformation function (YDBOcto#660).
		 * The cross reference key holds a converted value, which is not a subscript of the table's global. But every
		 * entry of the cross reference has, after the converted value, the subscripts of the global in their stored
		 * form, this column's included. So the key columns up to and including this one are advanced from the cross
		 * reference, as for a non-key column, and the key columns after it from the table's own global. One converted
		 * value can have several stored forms (e.g. the booleans 1 and "Y"), each of them a row of its own.
		 */
		num_xref_keys = 0;
		for (cur_key = 0; cur_key <= max_key; cur_key++) {
			advance_key_columns[cur_key] = key_columns[cur_key];
			if (key_columns[cur_key] == column) {
				num_xref_keys = cur_key + 1;
			}
		}
		assert(0 < num_xref_keys);
		num_advance_keys = max_key + 1;
	} else if (IS_KEY_COLUMN(column)) {
		/* The column is a primary key column (YDBOcto#660). The cross reference key takes its place, so the other
		 * "max_key" key columns follow it.
		 */
		num_xref_keys = get_key_column_xref_advance_keys(column, table, key_columns, max_key, advance_key_columns);
		num_advance_keys = max_key;
	} else {
		num_xref_keys = get_xref_advance_keys(column, table, key_columns, max_key, advance_key_columns);
		num_advance_keys = max_key + 1;
	}
	if (-1 == num_xref_keys) {
		return NULL; /* No usable shape (see "get_xref_advance_keys()") */
	}
	/* A non-key column's cross reference is followed by at least one primary key column. A key column's cross reference
	 * is followed by none when the column is the table's only key column: the cross reference key is then the only key.
	 */
	assert((0 < num_advance_keys) || IS_KEY_COLUMN(column));

	// Scan through and replace the table
	table_join = lp_get_table_join(plan);
	while (NULL != table_join) {
		assert(LP_TABLE_VALUE != table_join->v.lp_default.operand[0]->type); /* Caller should have ensured this */
		if (table_join->v.lp_default.operand[0]->type == LP_TABLE) {
			GET_LP(lp_table_alias, table_join, 0, LP_TABLE);
			if (lp_table_alias->v.lp_table.table_alias->unique_id == table_alias->unique_id)
				break;
		}
		GET_LP_ALLOW_NULL(table_join, table_join, 1, LP_TABLE_JOIN);
	}
	if (NULL == table_join) {
		/* Cannot happen: the callers ask for the cross reference of a column of a table in the FROM/JOIN list of
		 * "plan", so the loop above always finds its LP_TABLE_JOIN. This check is here only for clang-tidy: in a
		 * release build (e.g. RelWithDebInfo in the pipeline) the assert is compiled out, and the analyzer then
		 * reports the dereference of "table_join" below as a NULL pointer dereference.
		 */
		assert(FALSE);
		return NULL;
	}
	table_join->v.lp_default.operand[0] = lp_generate_xref_plan(table, column, unique_id);
	assert(NULL != table_join->v.lp_default.operand[0]); /* "lp_generate_xref_plan()" always returns a plan */
	lp_output_key = lp_get_output_key(table_join->v.lp_default.operand[0]);
	output_key = lp_output_key->v.lp_key.key;

	/* The first key is the cross reference key of the column */
	xref_key = lp_make_key(column_alias);
	xref_key->v.lp_key.key->is_cross_reference_key = TRUE;
	/* Determine whether the cross-reference should be stored in a YDB local or global variable. See comment for
	 * SqlKey.xref_prefix in logical_plan.h for additional background.
	 */
	SqlOptionalKeyword *source_keyword;
	source_keyword = get_keyword(column, OPTIONAL_GLOBAL);
	if (NULL == source_keyword) {
		UNPACK_SQL_STATEMENT(source_keyword, table->source, keyword);
	}
	xref_key->v.lp_key.key->xref_prefix
	    = ('^' == (source_keyword->v->v.value->v.string_literal[0]) ? PP_GLOBAL_PREFIX : PP_LOCAL_PREFIX);
	MALLOC_LP(keys, root, LP_KEYS);
	keys->v.lp_default.operand[0] = xref_key;
	/* The keys advanced from the cross reference follow it */
	for (cur_key = 0; cur_key < num_advance_keys; cur_key++) {
		SqlKey *advance_output_key;

		MALLOC_LP_2ARGS(keys->v.lp_default.operand[1], LP_KEYS);
		GET_LP(keys, keys, 1, LP_KEYS);

		/* A key column after a key column's cross reference is advanced from the table's global, as it was before
		 * the cross reference. Keep the plan's own key for it: a condition processed earlier may have fixed it
		 * (e.g. "c = 1" in "WHERE c = 1 AND b = 2" with keys (a, b, c)), and that condition has been removed from
		 * the WHERE since (YDBOcto#660).
		 */
		if (IS_KEY_COLUMN(column) && (cur_key >= num_xref_keys)) {
			keys->v.lp_default.operand[0] = find_table_key(plan, unique_id, advance_key_columns[cur_key]);
		} else {
			advance_output_key = (cur_key < num_xref_keys) ? output_key : NULL;
			keys->v.lp_default.operand[0] = lp_alloc_key(table, advance_key_columns[cur_key], unique_id, LP_KEY_ADVANCE,
								     advance_output_key, FALSE, NULL);
		}
	}
	return root;
}
