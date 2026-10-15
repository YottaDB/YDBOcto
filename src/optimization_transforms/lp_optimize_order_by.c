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

/* If the first ORDER BY column is a non-key column of a table, then walking an AIM cross reference of that column in
 * ascending (or descending) order produces the rows in ORDER BY order. In that case, replace the keys of that table in
 * the LP_KEYS list of "plan" with a cross reference key that walks ALL values of that column (sets
 * "key->is_order_by_xref_walk") followed by the primary key columns of the table. "lp_optimize_order_by()" then removes
 * the ORDER BY. With a LIMIT, only the first few rows of the cross reference are then read (YDBOcto#1160).
 *
 * Building the cross reference replaces the table in the LP_TABLE_JOIN list of "plan" and cannot be undone. Therefore all
 * checks below are done before that and the cross reference is built only if the entire ORDER BY can then be removed.
 */
static void insert_order_by_xref_walk_key(LogicalPlan *plan, LogicalPlan *lp_order_by_start) {
	LogicalPlan *lp_column_list, *lp_where, *lp_column_alias;

	/* "optimize_logical_plan()" optimizes each operand of a SET operation separately, and the plans without an LP_OUTPUT
	 * (e.g. LP_UPDATE) return early in "lp_optimize_order_by()". So this is always a SELECT query.
	 */
	assert(LP_SELECT_QUERY == plan->type);

	SqlTableAlias *root_table_alias;
	root_table_alias = plan->extra_detail.lp_select_query.root_table_alias;
	if ((NULL == root_table_alias) || root_table_alias->aggregate_function_or_group_by_or_having_specified) {
		/* With GROUP BY, HAVING or an aggregate function, the rows are output from the groups after the FOR loops
		 * end, not in the order the FOR loops produced them.
		 */
		return;
	}

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
	if (IS_KEY_COLUMN(column)) {
		/* A key column is walked by its own FOR loop. That is already handled in "lp_optimize_order_by()" for the
		 * types where M collation of the key column matches the ORDER BY order.
		 */
		return;
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
	SqlColumn *key_columns[MAX_KEY_COUNT];
	int	   max_key, key_num;
	memset(key_columns, 0, sizeof(key_columns));
	max_key = get_key_columns(table, key_columns);

	LogicalPlan *cur_order_by;
	GET_LP_ALLOW_NULL(cur_order_by, lp_order_by_start, 1, LP_ORDER_BY);
	for (key_num = 0; NULL != cur_order_by; key_num++) {
		SqlColumnAlias *order_by_column_alias;
		SqlColumn      *key_column;

		if (key_num > max_key) {
			return; /* More ORDER BY items than primary key columns */
		}
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
		key_column = key_columns[key_num];
		if (order_by_column_alias->column->v.column != key_column) {
			return; /* The ORDER BY item is not the next primary key column */
		}
		if (IS_COLUMN_STRING_TYPE(key_column)) {
			return; /* M collation of a VARCHAR key column is not ORDER BY order (YDBOcto#959) */
		}
		if (IS_DATE_TIME_DATA_TYPE(key_column->data_type_struct.data_type) && !table->readwrite) {
			return; /* A date/time key column of a READONLY table can be in non-standard form (YDBOcto#959) */
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
	/* If the first ORDER BY column is a non-key column, walk an AIM cross reference of that column (YDBOcto#1160).
	 * This can replace keys in LP_KEYS, so get the start of LP_KEYS only after this call.
	 */
	insert_order_by_xref_walk_key(plan, lp_order_by_start);
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
			LogicalPlan *lp_key, *lp_column_list, *lp_where;

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
				GET_LP(lp_column_list, cur_order_by, 0, LP_COLUMN_LIST);
				GET_LP(lp_where, lp_column_list, 0, LP_WHERE);

				/* ORDER BY and FOR loop order are guaranteed to be identical only for INTEGER, NUMERIC,
				 * BOOLEAN, readwrite table DATE/TIME types. Not for STRING type (for more details, see
				 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/959#note_1612645682) or READONLY TABLE
				 * DATE/TIME types.
				 * So skip optimization for the cases where ordering is not guaranteed. In the future, other types
				 * need to be examined on a case-by-case basis and either supported or not in the
				 * code block below.
				 */
				LogicalPlan *lp_column_list_alias;
				GET_LP(lp_column_list_alias, lp_where, 1, LP_COLUMN_LIST_ALIAS);
				if (0 == loop) {
					boolean_t type_supported;

					switch (lp_column_list_alias->v.lp_column_list_alias.column_list_alias->type) {
					case INTEGER_LITERAL:
					case NUMERIC_LITERAL:
					case BOOLEAN_VALUE:
						type_supported = TRUE;
						break;
					case STRING_LITERAL:
						type_supported = FALSE;
						break;
					case DATE_LITERAL:
					case TIME_LITERAL:
					case TIME_WITH_TIME_ZONE_LITERAL:
					case TIMESTAMP_LITERAL:
					case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
						/* A key column of a readonly table that is date/time type can be in non-standard
						 * form. A non standard form for date/time type
						 * can have values like '2023-01-01T01:01:01' and '2023-01-01 01:01:01' in the same
						 * global. Its ordering in FOR will not be the same one expected by ORDER BY.
						 * Do not optimize in this case.
						 */
						assert(key->column == sql_column);
						SqlTable *table;
						UNPACK_SQL_STATEMENT(table, sql_column->table, create_table);
						type_supported
						    = table->readwrite; // TRUE for readwrite and FALSE for readonly table
						break;
					default:
						assert(FALSE);
						type_supported = FALSE; /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
						break;
					}
					if (!type_supported) {
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
				case LP_BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX:;
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
