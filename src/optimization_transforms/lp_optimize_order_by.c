/****************************************************************
 *								*
 * Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/* Returns the SqlColumn of "key" if the ORDER BY column at "cur_order_by" is that same column of the same table (i.e. the
 * FOR loop of "key" can produce the rows in that ORDER BY column's order). Returns NULL otherwise.
 */
static SqlColumn *get_order_by_key_column(LogicalPlan *cur_order_by, SqlKey *key) {
	LogicalPlan *lp_column_list, *lp_where, *lp_column_alias;

	GET_LP(lp_column_list, cur_order_by, 0, LP_COLUMN_LIST);
	GET_LP(lp_where, lp_column_list, 0, LP_WHERE);
	lp_column_alias = lp_where->v.lp_default.operand[0];
	if (LP_COLUMN_ALIAS != lp_column_alias->type) {
		return NULL; /* The ORDER BY is not on a column alias */
	}

	SqlColumnAlias *column_alias;
	column_alias = lp_column_alias->v.lp_column_alias.column_alias;

	SqlStatement *column;
	column = column_alias->column;
	if (column_STATEMENT != column->type) {
		return NULL; /* The column alias is not on a SqlColumn */
	}
	if (key->unique_id != column_alias->table_alias_stmt->v.table_alias->unique_id) {
		return NULL; /* The ORDER BY and KEY column correspond to different tables */
	}

	SqlColumn *sql_column;
	sql_column = column->v.column;
	if (key->column != sql_column) {
		return NULL; /* KEY column is not same as ORDER BY column */
	}
	return sql_column;
}

/* Returns TRUE if "key" is a key column of a table that the WHERE (or ON) clause fixes to ONE value (with "=" or "IS").
 * Its FOR loop runs only once (per row of the outer tables). See the YDBOcto#1161 comment in "lp_optimize_order_by()".
 */
static boolean_t is_key_column_fixed_to_one_value(SqlKey *key) {
	if (LP_KEY_FIX != key->type) {
		return FALSE; /* The key is not fixed. Its FOR loop walks all values. */
	}
	if (key->is_cross_reference_key || (NULL == key->table)) {
		return FALSE; /* Not a key column of a table (a cross reference key or a key of a sub-query) */
	}
	return ((LP_BOOLEAN_EQUALS == key->fixed_to_value_type) || (LP_BOOLEAN_IS == key->fixed_to_value_type));
}

/* Test if the FOR loop of a key column returns its values in ORDER BY order (it depends on the column's type)
 *
 * Returns TRUE if the FOR loop of the key column "column" of "table", which visits its subscripts in M order, returns its
 * values in ORDER BY order. That is so for INTEGER and NUMERIC and, on a READWRITE table only, for BOOLEAN and the date/time
 * types. Not for a string (M orders canonical numbers before all other strings, see
 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/959#note_1612645682), and not for a date/time or
 * BOOLEAN key column of a READONLY table, whose global can hold the values in more than one form (e.g. "2023-01-01T01:01:01"
 * and "2023-01-01 01:01:01", or 1, "t" and "Y"; YDBOcto#959, YDBOcto#1179).
 */
static boolean_t is_key_column_loop_in_order_by_order(SqlTable *table, SqlColumn *column) {
	SqlDataType data_type;

	data_type = column->data_type_struct.data_type;
	if ((INTEGER_TYPE == data_type) || (NUMERIC_TYPE == data_type)) {
		return TRUE;
	}
	return (table->readwrite && ((BOOLEAN_TYPE == data_type) || IS_DATE_TIME_DATA_TYPE(data_type)));
}

/* Returns the first LP_ORDER_BY of "plan" if the query has both an ORDER BY and a LIMIT. Returns NULL otherwise. */
static LogicalPlan *get_order_by_of_query_with_limit(LogicalPlan *plan) {
	LogicalPlan *lp_out, *lp_order_by_start;

	lp_out = lp_get_output(plan);
	if (NULL == lp_out) {
		return NULL; /* Plan has no LP_OUTPUT (e.g. LP_UPDATE, LP_DELETE_FROM etc.), so no ORDER BY */
	}
	GET_LP_ALLOW_NULL(lp_order_by_start, lp_out, 1, LP_ORDER_BY);
	if (NULL == lp_order_by_start) {
		return NULL; /* No ORDER BY */
	}
	if (NULL == get_keyword_from_keywords(lp_get_select_keywords(plan)->v.lp_keywords.keywords, OPTIONAL_LIMIT)) {
		return NULL; /* No LIMIT */
	}
	return lp_order_by_start;
}

/* Test if a query has a LIMIT and the FOR loops of a table's key columns already give its ORDER BY order
 *
 * Returns TRUE if the query has a LIMIT, and the FOR loops of the key columns of the table "table_alias" (key column 0
 * first) already give the rows in ORDER BY order, starting with a key column before the one at "key_num" (YDBOcto#660).
 * E.g. "WHERE k >= 0 ORDER BY g, k LIMIT 2" with PRIMARY KEY (g, k): the FOR loop of "g", then that of "k" under each
 * "g", return the rows in ORDER BY order, so there is no sort, and the LIMIT stops the loops after two rows. A walk of
 * the cross reference of "k" through several values would return the rows grouped by "k" instead: every row it reads
 * would then have to be sorted before the first one is output. "use_key_column_xref()" does not use the cross reference
 * of the key column at "key_num" then.
 * The keys of other tables that come first must be fixed to one value (their FOR loops run once), and the ORDER BY
 * items must be the table's key columns in key order, each of a type whose FOR loop returns the values in ORDER BY
 * order, as "lp_optimize_order_by()" requires (see there). A key column fixed to one value matches its ORDER BY item,
 * if any, or none.
 */
boolean_t lp_is_order_by_with_limit_given_by_key_loops(LogicalPlan *plan, SqlTableAlias *table_alias, int key_num) {
	LogicalPlan *cur_order_by, *cur_keys;
	SqlTable    *table;
	boolean_t    key_before_matched;

	cur_order_by = get_order_by_of_query_with_limit(plan);
	if (NULL == cur_order_by) {
		/* No ORDER BY: the order of the rows does not matter. Or no LIMIT: every matching row is read anyway, and the
		 * cross reference reads fewer.
		 */
		return FALSE;
	}

	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	key_before_matched = FALSE;
	for (cur_keys = lp_get_keys(plan); (NULL != cur_keys) && (NULL != cur_order_by);) {
		LogicalPlan *lp_key;
		SqlKey	    *key;
		SqlColumn   *column;

		GET_LP(lp_key, cur_keys, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		GET_LP_ALLOW_NULL(cur_keys, cur_keys, 1, LP_KEYS);
		if (key->unique_id != table_alias->unique_id) {
			if (is_key_column_fixed_to_one_value(key)) {
				continue; /* Its FOR loop runs once, so it does not change the order of the rows */
			}
			return FALSE; /* A FOR loop of another table is outside or between those of this table */
		}
		if (key->is_cross_reference_key || (NULL == key->column)) {
			/* The table already has a cross reference, e.g. the one that "lp_optimize_order_by_with_limit()" walks
			 * for "ORDER BY a LIMIT 2" on a string first key column "a": its FOR loops are not over its own global.
			 */
			return FALSE;
		}
		if (is_key_column_fixed_to_one_value(key)) {
			if (NULL != get_order_by_key_column(cur_order_by, key)) {
				GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
			}
			continue; /* Its FOR loop runs once: the next key column gives the order */
		}
		column = get_order_by_key_column(cur_order_by, key);
		if (NULL == column) {
			return FALSE; /* The ORDER BY item is not this key column: the FOR loops do not give the ORDER BY order */
		}
		if (!is_key_column_loop_in_order_by_order(table, column)) {
			return FALSE; /* The FOR loop of this key column does not return its values in ORDER BY order */
		}
		if (lp_get_key_num(column) < key_num) {
			key_before_matched = TRUE;
		}
		GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
	}
	/* All ORDER BY items are served by the FOR loops, and one of them is a key column before the one at "key_num" */
	return (NULL == cur_order_by) && key_before_matched;
}

/* Returns TRUE if the AIM cross reference of "column" (of "table") can have subscripts that do not collate in the order of the
 * column values. That is the case for an INTEGER or NUMERIC column of a READONLY table: the cross reference stores each value
 * as it is in the global, and a value that is not a canonical number (e.g. "-0.25" or "010") is then a string subscript,
 * which collates after all numbers (YDBOcto#1168). Walking such a cross reference would return the rows in the wrong order.
 */
static boolean_t is_xref_order_unreliable(SqlTable *table, SqlColumn *column) {
	SqlDataType data_type;

	data_type = column->data_type_struct.data_type;
	return (!table->readwrite && ((INTEGER_TYPE == data_type) || (NUMERIC_TYPE == data_type)));
}

/* Walk a cross reference of the first ORDER BY column instead of sorting, if the walk gives the ORDER BY order
 *
 * If the first ORDER BY column is a non-key column of a table, then walking an AIM cross reference of that column in
 * ascending (or descending) order produces the rows in ORDER BY order. In that case, replace the keys of that table in
 * the LP_KEYS list of "plan" with a cross reference key that walks ALL values of that column (sets
 * "key->is_order_by_xref_walk") followed by the primary key columns of the table. "lp_optimize_order_by()" then removes
 * the ORDER BY. With a LIMIT, only the first few rows of the cross reference are then read (YDBOcto#1160).
 *
 * The first ORDER BY column can also be a primary key column (YDBOcto#660), walked through its AIM subscript cross
 * reference, when the table's own global does not give its order: a key column after the first (the FOR loops of the
 * key columns before it are outside its own), of any type, or the first key column if it is a string (the table's global
 * holds it in M collation, where canonical numbers come before all other strings, YDBOcto#959/#397). With a LIMIT, the
 * walk is inserted before the WHERE clause, which can then fix the key columns after it, as they are reached through the
 * table's global after the cross reference ("lp_optimize_order_by()" skips them as keys fixed to one value).
 *
 * "before_where" is TRUE for the call made before the WHERE and ON conditions fix any key (see
 * "lp_optimize_order_by_with_limit()"), FALSE for the call made after them.
 *
 * Building the cross reference replaces the table in the LP_TABLE_JOIN list of "plan" and cannot be undone. Therefore all
 * checks below are done before that and the cross reference is built only if the entire ORDER BY can then be removed.
 */
static void insert_order_by_xref_walk_key(LogicalPlan *plan, LogicalPlan *lp_order_by_start, boolean_t before_where) {
	LogicalPlan *lp_column_list, *lp_where, *lp_column_alias;

	/* "optimize_logical_plan()" optimizes each operand of a SET operation separately, and the plans without an LP_OUTPUT
	 * (e.g. LP_UPDATE) return early in "lp_optimize_order_by()". So this is always a SELECT query.
	 */
	assert(LP_SELECT_QUERY == plan->type);
	/* The caller returns early for a query with GROUP BY, HAVING or an aggregate function */
	assert(!plan->extra_detail.lp_select_query.root_table_alias->aggregate_function_or_group_by_or_having_specified);

	/* The first ORDER BY item has to be a column of a table (not an expression or a sub-query column) */
	GET_LP(lp_column_list, lp_order_by_start, 0, LP_COLUMN_LIST);
	GET_LP(lp_where, lp_column_list, 0, LP_WHERE);
	lp_column_alias = lp_where->v.lp_default.operand[0];
	if (LP_COLUMN_ALIAS != lp_column_alias->type) {
		return; /* The ORDER BY is on an expression, which has no cross reference */
	}

	SqlColumnAlias *column_alias;
	column_alias = lp_column_alias->v.lp_column_alias.column_alias;
	if (column_STATEMENT != column_alias->column->type) {
		return; /* The ORDER BY is on "table.*", which is not one column */
	}

	/* The column references of a sub-query, VALUES clause or view in the FROM/JOIN list are LP_DERIVED_COLUMN, not
	 * LP_COLUMN_ALIAS (see "lp_replace_derived_table_references()"). So this column is in a CREATE TABLE.
	 */
	SqlTableAlias *table_alias;
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
	assert(create_table_STATEMENT == table_alias->table->type);

	SqlColumn *column;
	SqlTable  *table;
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	int walk_key_num; /* The KEY NUM of the column, or -1 for a non-key column */
	walk_key_num = -1;
	if (IS_KEY_COLUMN(column)) {
		walk_key_num = lp_get_key_num(column);
		if ((0 == walk_key_num) && !IS_COLUMN_STRING_TYPE(column) && !IS_XREF_VALUE_TRANSFORMED(table, column)) {
			/* The first key column is walked by its own FOR loop over the table's global, in the ORDER BY order for
			 * the types "lp_optimize_order_by()" checks (YDBOcto#959). Not when its cross reference holds the values
			 * converted by a transformation function: the global holds them in their stored form, whose order can
			 * differ, and the cross reference gives the order of the converted values.
			 */
			return;
		}
	} else if (before_where) {
		/* Before the WHERE clause, only a key column's walk is considered (see "lp_optimize_order_by_with_limit()") */
		return;
	}
	if (before_where) {
		/* A condition on the column, or on a key column before it, is served better by the WHERE clause: through a
		 * cross reference walk over its values (e.g. a LIKE prefix), or by fixing that key column in place.
		 */
		LogicalPlan *table_join;

		if (lp_is_key_column_compared(lp_get_select_where(plan)->v.lp_default.operand[0], table_alias->unique_id,
					      walk_key_num, NULL)) {
			return;
		}
		for (table_join = lp_get_table_join(plan); NULL != table_join;) {
			if (lp_is_key_column_compared(table_join->extra_detail.lp_table_join.join_on_condition,
						      table_alias->unique_id, walk_key_num, NULL)) {
				return;
			}
			GET_LP_ALLOW_NULL(table_join, table_join, 1, LP_TABLE_JOIN);
		}
	}
	if (table->has_iterator) {
		return; /* Tables with an ITERATOR cannot be cross referenced */
	}
	if (is_xref_order_unreliable(table, column)) {
		return; /* The cross reference of this column might not be in the order of its values (YDBOcto#1168) */
	}

	SqlOptionalKeyword *source_keyword;
	source_keyword = get_keyword(column, OPTIONAL_GLOBAL);
	if (NULL == source_keyword) {
		UNPACK_SQL_STATEMENT(source_keyword, table->source, keyword);
	}
	if ('^' != source_keyword->v->v.value->v.string_literal[0]) {
		/* The column lives in a local variable, so its cross reference would too. Only an AIM cross reference (of a
		 * column in a global) stores a string value as "#"_value, which collates in ORDER BY order (canonical numbers
		 * would otherwise sort before all other strings, YDBOcto#397). Keep it simple and use only AIM cross references.
		 */
		return;
	}

	/* With a RIGHT or FULL JOIN, the rows of the right table that matched no row of the left table are output after all
	 * other rows (by the second half of the join). That breaks the order of the cross reference walk.
	 * The table also cannot be the right side of a LEFT JOIN, as only the first table is eligible (see the LP_KEYS check
	 * below) and the first table in the LP_TABLE_JOIN list is never the right side of a join.
	 */
	LogicalPlan *table_join;
	table_join = lp_get_table_join(plan);
	do {
		enum SqlJoinType join_type;

		join_type = table_join->extra_detail.lp_table_join.cur_join_type;
		if ((RIGHT_JOIN == join_type) || (FULL_JOIN == join_type)) {
			return;
		}
		GET_LP_ALLOW_NULL(table_join, table_join, 1, LP_TABLE_JOIN);
	} while (NULL != table_join);

	/* The table's FOR loops have to be the outermost ones, i.e. the first keys in LP_KEYS, except for keys that are fixed
	 * to one value (their FOR loop runs only once, YDBOcto#1161). The table must also not have any key fixed or a cross
	 * reference already (e.g. from a WHERE clause on another column). That WHERE clause already chose how to walk the
	 * table.
	 */
	LogicalPlan *cur_keys;
	boolean_t    found_table_keys;
	found_table_keys = FALSE;
	for (cur_keys = lp_get_keys(plan); NULL != cur_keys;) {
		LogicalPlan *lp_key;
		SqlKey	    *key;

		GET_LP(lp_key, cur_keys, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		if (key->unique_id == table_alias->unique_id) {
			if (key->is_order_by_xref_walk) {
				return; /* Inserted before the WHERE clause (see "lp_optimize_order_by_with_limit()") */
			}
			if (LP_KEY_ADVANCE != key->type) {
				return; /* This table already has a fixed key or a cross reference (whose key is fixed) */
			}
			/* A cross reference key is fixed by the WHERE clause that built it, and it comes before the keys of the
			 * primary key columns that it outputs. So neither of them gets here.
			 */
			assert(!key->is_cross_reference_key && (NULL == key->cross_reference_output_key));
			found_table_keys = TRUE;
		} else if (found_table_keys) {
			break; /* Done with the keys of this table */
		} else if (!is_key_column_fixed_to_one_value(key)) {
			return; /* A FOR loop of another table is outside the FOR loops of this table */
		}
		GET_LP_ALLOW_NULL(cur_keys, cur_keys, 1, LP_KEYS);
	}
	/* The ORDER BY column is a column of a table in the FROM/JOIN list, so that table has keys in LP_KEYS */
	assert(found_table_keys);

	/* The cross reference walk returns the rows in the order of the ORDER BY column and, within one value of that
	 * column, in the order of the primary key columns (e.g. "names" rows 0 and 5, both with lastname "Cool"). So the walk
	 * gives the ORDER BY order only if the ORDER BY items after the first one (if any) are the leading primary key
	 * columns of the same table, in key order. For "names" (PRIMARY KEY (id)), "ORDER BY lastname" and
	 * "ORDER BY lastname, id" qualify, but "ORDER BY lastname, firstname" does not. For PRIMARY KEY (a, b), "ORDER BY x",
	 * "ORDER BY x, a" and "ORDER BY x, a, b" qualify, but "ORDER BY x, b" does not.
	 * The FOR loops of those key columns $ORDER through their subscripts as stored, so the M collation of each one also
	 * has to be the ORDER BY order (see the type checks in "lp_optimize_order_by()").
	 * All of this is checked before the cross reference is built, as building it replaces the keys of the table and
	 * cannot be undone. If the ORDER BY could then still not be removed, the query would pay for the walk and the sort.
	 */
	SqlColumn *key_columns[MAX_KEY_COUNT], *walk_key_columns[MAX_KEY_COUNT];
	int	   max_key, key_num, num_walk_keys;
	memset(key_columns, 0, sizeof(key_columns));
	max_key = get_key_columns(table, key_columns);
	/* The primary key columns that the walk visits, in order: all of them for a non-key column; the others for a key
	 * column (YDBOcto#660), those before it from the cross reference and then those after it from the table's global.
	 * A key column whose cross reference holds converted values is visited again in its stored form, from the cross
	 * reference, after those before it (see "lp_generate_xref_keys()").
	 */
	num_walk_keys = 0;
	for (key_num = 0; key_num <= max_key; key_num++) {
		if ((key_columns[key_num] != column) || IS_XREF_VALUE_TRANSFORMED(table, column)) {
			walk_key_columns[num_walk_keys++] = key_columns[key_num];
		}
	}

	LogicalPlan *cur_order_by;
	GET_LP_ALLOW_NULL(cur_order_by, lp_order_by_start, 1, LP_ORDER_BY);
	for (key_num = 0; NULL != cur_order_by; key_num++) {
		SqlColumnAlias *order_by_column_alias;
		SqlColumn      *key_column;

		GET_LP(lp_column_list, cur_order_by, 0, LP_COLUMN_LIST);
		GET_LP(lp_where, lp_column_list, 0, LP_WHERE);
		lp_column_alias = lp_where->v.lp_default.operand[0];
		if (LP_COLUMN_ALIAS != lp_column_alias->type) {
			return; /* The ORDER BY item is an expression */
		}
		order_by_column_alias = lp_column_alias->v.lp_column_alias.column_alias;
		if (column_STATEMENT != order_by_column_alias->column->type) {
			return; /* The ORDER BY item is "table.*", which is not one column */
		}
		if (order_by_column_alias->table_alias_stmt->v.table_alias->unique_id != table_alias->unique_id) {
			return; /* The ORDER BY item is a column of another table */
		}
		if (key_num == num_walk_keys) {
			return; /* More ORDER BY items than primary key columns */
		}
		key_column = walk_key_columns[key_num];
		if (order_by_column_alias->column->v.column != key_column) {
			return; /* The ORDER BY item is not the next primary key column */
		}
		if (!is_key_column_loop_in_order_by_order(table, key_column)) {
			return; /* The FOR loop of this key column does not return its values in ORDER BY order */
		}
		GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
	}

	SqlKey *xref_key;
	xref_key = lp_insert_xref_key(plan, table, column_alias, table_alias);
	if (NULL == xref_key) {
		return; /* The column cannot be cross referenced (e.g. an EXTRACT column). The plan is untouched. */
	}
	assert('^' == xref_key->xref_prefix[0]);
	xref_key->is_order_by_xref_walk = TRUE;
	return;
}

/* Let the ORDER BY of a query with a LIMIT choose a cross reference walk before the WHERE and ON conditions do
 *
 * With a LIMIT, the ORDER BY of a query chooses how to walk a table before its WHERE and ON conditions do (YDBOcto#660).
 * Called before those conditions fix any key: if the first ORDER BY column is a key column whose cross reference walk
 * gives the ORDER BY order, the walk is inserted now, and the conditions on the other key columns of the table are then
 * checked under it. A LIMIT stops the walk after the first few rows that pass them, without sorting all the rows that
 * the conditions select. Without a LIMIT, the conditions choose first, and the ORDER BY is considered after them
 * ("lp_optimize_order_by()").
 * The walk is not inserted if a condition compares the column or a key column before it (the conditions then serve those
 * columns better), and only for a key column: for a non-key column, the conditions still choose first.
 */
void lp_optimize_order_by_with_limit(LogicalPlan *plan) {
	LogicalPlan *lp_order_by_start;

	lp_order_by_start = get_order_by_of_query_with_limit(plan);
	if (NULL == lp_order_by_start) {
		return; /* No ORDER BY, or no LIMIT: the WHERE and ON conditions choose first */
	}
	assert(LP_SELECT_QUERY == plan->type);
	if (plan->extra_detail.lp_select_query.root_table_alias->aggregate_function_or_group_by_or_having_specified) {
		/* With GROUP BY, HAVING or an aggregate function, the rows are output from the groups after the FOR loops
		 * end, so no cross reference walk gives the ORDER BY order (see "lp_optimize_order_by()", YDBOcto#1193)
		 */
		return;
	}
	insert_order_by_xref_walk_key(plan, lp_order_by_start, TRUE);
}

/* This function tries to optimize any ORDER BY usage by seeing if it is possible to remove the LP_ORDER_BY logical plan
 * altogether. For example, in the query "select * from names order by id;", it is possible to remove the "order by id"
 * because the FOR loop emitted in the M code would be in ascending order of "id" anyways and the M ordering of the
 * integer "id" column would match the ORDER BY ordering of that column so we can avoid an unnecessary extra ORDER BY step.
 * (YDBOcto#959)
 */
void lp_optimize_order_by(LogicalPlan *plan) {
	LogicalPlan *lp_keys_start, *lp_out, *lp_order_by_start;
	LogicalPlan *cur_keys, *cur_order_by;

	lp_out = lp_get_output(plan);
	if (NULL == lp_out) {
		return; /* Plan has no LP_OUTPUT (e.g. LP_UPDATE, LP_DELETE_FROM etc.). No optimization possible in that case. */
	}
	GET_LP_ALLOW_NULL(lp_order_by_start, lp_out, 1, LP_ORDER_BY);
	if (NULL == lp_order_by_start) {
		return; /* No ORDER BY present to try optimization */
	}
	assert(LP_TABLE_VALUE != plan->type);

	SqlTableAlias *root_table_alias;
	assert(LP_SELECT_QUERY == plan->type);
	root_table_alias = plan->extra_detail.lp_select_query.root_table_alias;
	assert(NULL != root_table_alias);
	if (root_table_alias->aggregate_function_or_group_by_or_having_specified) {
		/* With GROUP BY, HAVING or an aggregate function, the rows are output from the groups after the FOR loops
		 * end, not in the order the FOR loops produced them. So no FOR loop order, of the table's global or of a
		 * cross reference, gives the ORDER BY order: keep the sort (YDBOcto#1193).
		 */
		return;
	}

	/* If the first ORDER BY column is a non-key column, walk an AIM cross reference of that column (YDBOcto#1160).
	 * This can replace keys in LP_KEYS, so get the start of LP_KEYS only after this call.
	 */
	insert_order_by_xref_walk_key(plan, lp_order_by_start, FALSE);
	lp_keys_start = lp_get_keys(plan);
	assert(NULL != lp_keys_start);
	/* The first iteration of the below loop is to see if the optimization is possible.
	 *    If not, we break out of the for loop. If yes, we continue on to the second iteration of the loop.
	 * The second iteration of the below loop is to set "key->emit_desc_order" so template processing logic
	 *    can then generate the FOR loop for the key columns in the right order (ascending or descending order).
	 */
	int loop;
	for (loop = 0; loop < 2; loop++) {
		cur_keys = lp_keys_start;
		cur_order_by = lp_order_by_start;
		for (;;) {
			LogicalPlan *lp_key;

			assert(NULL != cur_keys);
			assert(NULL != cur_order_by);
			GET_LP(lp_key, cur_keys, 0, LP_KEY);

			SqlKey *key;
			key = lp_key->v.lp_key.key;

			if (is_key_column_fixed_to_one_value(key)) {
				/* This is a key column of a table that the WHERE (or ON) clause fixes to ONE value, e.g.
				 * "test_code" in "WHERE test_code = 'PSA' ORDER BY isn" with PRIMARY KEY (test_code, isn). Its FOR
				 * loop runs once (per row of the outer tables), so the FOR loop of the next key determines the
				 * output order (YDBOcto#1161).
				 *
				 * If the current ORDER BY column is this key column, then it is ordering on a value that does not
				 * change. Consume that ORDER BY column, whatever its type and direction. Otherwise, skip this key
				 * and match the same ORDER BY column against the next key.
				 */
				if (NULL != get_order_by_key_column(cur_order_by, key)) {
					GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
				}
			} else if (key->is_order_by_xref_walk) {
				/* This is the cross reference key that "insert_order_by_xref_walk_key()" created for the first
				 * ORDER BY column (YDBOcto#1160). It walks the cross reference in ORDER BY order, so it matches
				 * that column for any type and either direction.
				 */
				assert(LP_KEY_ADVANCE == key->type);
				/* The key was created for the first ORDER BY column. The keys before it are of other tables (see
				 * "insert_order_by_xref_walk_key()"), so none of them consumed that column.
				 */
				assert(NULL != get_order_by_key_column(cur_order_by, key));
				if (1 == loop) {
					key->emit_desc_order = (OPTIONAL_DESC == cur_order_by->extra_detail.lp_order_by.direction);
				}
				GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
			} else if ((LP_KEY_FIX != key->type) || !key->is_cross_reference_key) {
				assert(!key->emit_desc_order); /* assert this is initialized to FALSE at key allocation time */
				assert((NULL != key->table) || (NULL == key->column));
				assert((NULL == key->table) || (NULL != key->column));
				if (NULL == key->table) {
					/* The key does not correspond to a SqlTable. Cannot optimize. */
					assert(0 == loop);
					break;
				}

				SqlColumn *sql_column;
				sql_column = get_order_by_key_column(cur_order_by, key);
				if (NULL == sql_column) {
					/* The ORDER BY column is not this KEY column (see "get_order_by_key_column()" for the
					 * details). So the FOR loop of this key does not produce rows in the ORDER BY order. Cannot
					 * optimize.
					 */
					assert(0 == loop);
					break;
				}

				/* ORDER BY and FOR loop order are guaranteed to be identical only for INTEGER, NUMERIC, and
				 * readwrite table BOOLEAN and DATE/TIME types (see "is_key_column_loop_in_order_by_order()"). Skip
				 * the optimization for the cases where the ordering is not guaranteed.
				 */
				if (0 == loop) {
					assert(key->column == sql_column);
					if (!is_key_column_loop_in_order_by_order(key->table, sql_column)) {
						break;
					}
				}

				enum OptionalKeyword direction;
				direction = cur_order_by->extra_detail.lp_order_by.direction;
				assert((OPTIONAL_ASC == direction) || (OPTIONAL_DESC == direction));
				if (OPTIONAL_DESC == direction) {
					if (0 == loop) {
						if (NULL != get_keyword(sql_column, OPTIONAL_END)) {
							/* Cannot optimize ORDER BY DESC if END keyword is there */
							break;
						}
					} else {
						key->emit_desc_order = TRUE;
					}
				}
				GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
			} else {
				assert((NULL != key->fixed_to_value) && key->is_cross_reference_key);
				assert((LP_COLUMN_LIST != key->fixed_to_value->type)
				       || (LP_BOOLEAN_IN == key->fixed_to_value_type));

				boolean_t is_order_by_column;
				is_order_by_column = (NULL != get_order_by_key_column(cur_order_by, key));

				boolean_t ok_to_optimize;
				switch (key->fixed_to_value_type) {
				case LP_BOOLEAN_EQUALS:
				case LP_BOOLEAN_IS:
					/* This xref key is fixed to ONE value. Skip it for the purposes of the ORDER BY
					 * optimization. If the current ORDER BY column is the cross referenced column, it is
					 * ordering on a value that does not change, so consume that ORDER BY column too (same as a
					 * key column fixed to one value, YDBOcto#1161).
					 */
					if (is_order_by_column) {
						GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
					}
					ok_to_optimize = TRUE;
					break;
				case LP_BOOLEAN_IN:
					/* If this is fixed to a LIST of values (i.e. IN operator) then we cannot optimize ORDER BY
					 * as the processing order would be rows sorted on the cross referenced non-key column value
					 * in the order of the IN list, which is not the same as the ORDER BY order (within one
					 * non-key column value, all rows will be sorted in primary key column order, but the
					 * primary key column values corresponding to the first non-key column value are not
					 * guaranteed to be in sorted order compared to the primary key column values corresponding
					 * to the second (or later) non-key column values).
					 */
					ok_to_optimize = FALSE;
					break;
				case LP_BOOLEAN_LESS_THAN:
				case LP_BOOLEAN_GREATER_THAN:
				case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
				case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
				case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY:
				case LP_BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX:
				case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX:
				case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX_WALK:;
					/* This is an xref key that is used for a range of values. The FOR loop walks the cross
					 * reference over that range, in the collation order of the cross reference. That is the
					 * ORDER BY order if the ORDER BY column is the cross referenced column itself
					 * (YDBOcto#1160), and if that collation order is the ORDER BY order: for an AIM cross
					 * reference (which stores a string value as "#"_value) except for an INTEGER or NUMERIC
					 * column of a READONLY table (see "is_xref_order_unreliable()"), but for a cross reference
					 * in a local variable only for a non-string column (canonical numbers sort before all other
					 * strings there, YDBOcto#397). NULL values are never in a range, so their position in the
					 * cross reference does not matter.
					 *
					 * Otherwise disable the ORDER BY optimization for the same reasons as described in the
					 * previous comment block (LIST of values).
					 */
					enum OptionalKeyword direction;
					direction = cur_order_by->extra_detail.lp_order_by.direction;
					ok_to_optimize = is_order_by_column
							 && (('^' == key->xref_prefix[0]) || !IS_COLUMN_STRING_TYPE(key->column))
							 && !is_xref_order_unreliable(key->table, key->column);
					if (!ok_to_optimize) {
						break;
					}
					if (1 == loop) {
						key->emit_desc_order = (OPTIONAL_DESC == direction);
					}
					GET_LP_ALLOW_NULL(cur_order_by, cur_order_by, 1, LP_ORDER_BY);
					break;
				default:
					assert(FALSE);
					ok_to_optimize = FALSE;
					break;
				}
				if (!ok_to_optimize) {
					break;
				}
			}
			GET_LP_ALLOW_NULL(cur_keys, cur_keys, 1, LP_KEYS);
			if (NULL == cur_keys) {
				break;
			}
			if (NULL == cur_order_by) {
				break;
			}
		}
		if (NULL != cur_order_by) {
			/* There are 2 possibilities.
			 * 1) We did a "break" BEFORE the "GET_LP_ALLOW_NULL(cur_order_by,...)" call in the "for" loop above.
			 *    It means we decided to disable the optimization. Therefore "return".
			 * 2) We did a "break" AFTER  the "GET_LP_ALLOW_NULL(cur_order_by,...)" call in the "for" loop above.
			 *    It means there are ORDER BY columns that don't have any matching KEYS in the FROM/JOIN
			 *    table list. Output order cannot be preserved using just KEYS order (FOR loop) in M code.
			 *    Therefore disable ORDER BY optimization.
			 */
			assert(0 == loop);
			return;
		}
	}
	/* If we reach here, it means the ORDER BY optimization was possible and the ASC/DESC direction in the ORDER BY
	 * has been moved to the corresponding LP_KEY structures in the plan. Hence remove all LP_ORDER_BY plans.
	 * This lets LIMIT in SELECT queries work faster (YDBOcto#959).
	 */
	lp_out->v.lp_default.operand[1] = NULL;
	return;
}
