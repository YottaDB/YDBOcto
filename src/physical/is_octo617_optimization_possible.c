/****************************************************************
 *								*
 * Copyright (c) 2024-2026 YottaDB LLC and/or its subsidiaries.	*
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
#include "physical_plan.h"

/* Test if a key column keyword excludes entries of a cross reference that MIN/MAX cannot walk past
 *
 * Returns TRUE if a key column of the table that "pplan" reads excludes subscripts of the table's global from the table in a
 * way that the AIM cross reference of "aggr_column" does not express, and that "tmpl_octo617_xref_walk" cannot
 * walk past, so that its first or last value may not be in a row. AIM builds the cross reference over the range
 * "START:ENDPOINT" of each key column, inclusive at both ends, so it also indexes the START subscript, which is not a row,
 * and the subscripts that SKIP, SKIPCONDITION and END, which are conditions rather than ranges, exclude. The template
 * walks the key subscripts of the cross reference under each value past them, when they are those of every key column;
 * when the column's GLOBAL lists only some of them, every row is read. START with STARTINCLUDE, and ENDPOINT, are the ends
 * of the range the cross reference is built over, so they need nothing.
 */
static boolean_t has_key_column_filter(PhysicalPlan *pplan, SqlColumn *aggr_column) {
	unsigned int i;

	/* The cross reference of a column with no GLOBAL of its own is built on the table's GLOBAL, which lists every key
	 * column in key order (ERR_GLOBAL_KEY_COLS_ORDER). A column's own GLOBAL may list only some of them (YDBOcto#1107). The
	 * caller has checked that it lists the first key columns of the table, in key order
	 * ("get_column_global_key_prefix()"). So it lists them all when it lists as many as the table has.
	 */
	if (get_column_global_key_prefix(aggr_column, pplan->iterKeys[0]->table) == (int)pplan->total_iter_keys) {
		return FALSE; /* The walk in the template applies START, SKIP and SKIPCONDITION */
	}
	for (i = 0; i < pplan->total_iter_keys; i++) {
		if (IS_XREF_KEY_COLUMN_FILTERED(pplan->iterKeys[i]->column)) {
			return TRUE;
		}
	}
	return FALSE;
}

boolean_t is_octo617_optimization_possible(PhysicalPlan *pplan) {
	if (LP_SELECT_QUERY != pplan->lp_select_query->type) {
		return FALSE; /* optimization is possible only in SELECT queries, not INSERT INTO, DELETE etc. */
	}
	/* Check if simple aggregate function usages on table columns can be optimized (YDBOcto#617).
	 * See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/617#note_2142141907 for more details.
	 */
	assert(NULL != pplan->tablejoin);
	assert(LP_TABLE_JOIN == pplan->tablejoin->type);
	assert(NO_JOIN == pplan->tablejoin->extra_detail.lp_table_join.cur_join_type);
	if (NULL != pplan->tablejoin->v.lp_default.operand[1]) {
		/* There is at least one JOIN involved. That means there are at least 2 tables involved. So cannot optimize. */
		return FALSE;
	}
	if (pplan->where && pplan->where->v.lp_default.operand[0]) {
		/* This query has a WHERE clause specified. Cannot optimize. */
		return FALSE;
	}

	unsigned int iter_key_index;
	for (iter_key_index = 0; iter_key_index < pplan->total_iter_keys; iter_key_index++) {
		SqlKey *iter_key;

		iter_key = pplan->iterKeys[iter_key_index];
		if ((NULL != iter_key->fixed_to_value) || iter_key->is_cross_reference_key) {
			/* The query had a WHERE clause, which the key fixing used up: it fixed a key column to a value
			 * (e.g. "WHERE a = 2") or replaced the keys with a cross reference (e.g. "WHERE firstname = 'Acid'"),
			 * and removed the condition from the WHERE. MIN/MAX are then those of the rows the WHERE keeps, not of
			 * the table. Cannot optimize.
			 */
			return FALSE;
		}
	}
	if (GROUP_BY_SPECIFIED & pplan->aggregate_function_or_group_by_or_having_specified) {
		/* This query has a GROUP BY clause specified. Cannot optimize. */
		return FALSE;
	}
	if (HAVING_SPECIFIED & pplan->aggregate_function_or_group_by_or_having_specified) {
		/* This query has a HAVING clause specified. Cannot optimize. */
		return FALSE;
	}
	if (!(AGGREGATE_FUNCTION_SPECIFIED & pplan->aggregate_function_or_group_by_or_having_specified)) {
		/* This query has no aggregate function usages. Cannot optimize. */
		return FALSE;
	}
	assert(IS_GROUP_BY_PLAN(pplan));

	LogicalPlan *lp_table;
	lp_table = pplan->tablejoin->v.lp_default.operand[0];
	/* The FROM can also be a sub-query or a VALUES clause, which are not of type LP_TABLE */
	if ((LP_TABLE == lp_table->type) && (create_table_STATEMENT == lp_table->v.lp_table.table_alias->table->type)
	    && lp_table->v.lp_table.table_alias->table->v.create_table->has_iterator) {
		/* The ITERATOR decides which subscripts of the table's global are rows (with VIRTUAL, the key values need not be
		 * subscripts of the global at all). Neither the subscripts of the global nor its AIM cross references, which
		 * index every node of the global, give the first or last value of a row, or tell whether the table has a row
		 * (which the MIN/MAX of a literal needs). The WHERE and ORDER BY paths do not cross reference such a table
		 * either.
		 */
		return FALSE;
	}

	LogicalPlan *af, *first_aggregate;
	first_aggregate = pplan->lp_select_query->extra_detail.lp_select_query.first_aggregate;
	for (af = first_aggregate; NULL != af; af = af->extra_detail.lp_aggregate_function.next_aggregate) {
		switch (af->type) {
		case LP_AGGREGATE_FUNCTION_COUNT_ASTERISK:
		case LP_AGGREGATE_FUNCTION_COUNT:
		case LP_AGGREGATE_FUNCTION_SUM:
		case LP_AGGREGATE_FUNCTION_AVG:
		case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT:
		case LP_AGGREGATE_FUNCTION_SUM_DISTINCT:
		case LP_AGGREGATE_FUNCTION_AVG_DISTINCT:
		case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK:
		case LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK:
			/* There is at least one aggregate function that is not MIN/MAX. Cannot apply YDBOcto#617 optimization. */
			return FALSE;
			break;
		case LP_AGGREGATE_FUNCTION_MIN:
		case LP_AGGREGATE_FUNCTION_MAX:;
			LogicalPlan *lp_column_list;
			LogicalPlan *l_plan;

			GET_LP(lp_column_list, af, 0, LP_COLUMN_LIST);
			l_plan = lp_column_list->v.lp_default.operand[0];
			if (LP_VALUE == l_plan->type) {
				/* There is a MIN/MAX aggregate function usage on a literal.
				 * Can still apply YDBOcto#617 optimization.
				 */
				break;
			}
			if (LP_COLUMN_ALIAS != l_plan->type) {
				/* There is a MIN/MAX aggregate function usage on something that is not a column value
				 * or a literal. Cannot apply YDBOcto#617 optimization. */
				return FALSE;
			}

			SqlColumnAlias *column_alias;
			column_alias = l_plan->v.lp_column_alias.column_alias;
			if (column_STATEMENT != column_alias->column->type) {
				/* Not sure how this is possible. Cannot anyways apply the YDBOcto#617 optimization in this case. */
				assert(FALSE);
				return FALSE;
			}
			SqlKey	  *key;
			SqlColumn *aggr_column, *key_column;
			SqlTable  *table;
			boolean_t  is_composite_key, use_xref;
			aggr_column = column_alias->column->v.column;
			key = pplan->iterKeys[0];
			key_column = key->column;
			table = key->table;
			is_composite_key = (1 < pplan->total_iter_keys);
			assert(NULL == af->extra_detail.lp_aggregate_function.octo617_xref_plan);
			/* The first or last value of the column comes from its AIM cross reference ("use_xref"), or, for the key
			 * column of a table with one key column, from the table's global, whose subscripts are those values. The
			 * cross reference is needed:
			 *   - for a non-key column;
			 *   - for any column of a table with several key columns (YDBOcto#660): the first or last subscript of its
			 *     global at one level need not have a row under it (e.g. ^T(9) with only ^T(9,1,"X")), whereas the
			 *     cross reference of a key column (XREFSUB^%YDBAIM) indexes only the nodes that are rows;
			 *   - for a key column whose cross reference holds its values converted by a transformation function: a
			 *     date/time key column of a READONLY table, whose global holds the values in a form that need not sort
			 *     in their order (e.g. "2023-01-01T01:01:01" after "2023-01-01 02:01:01", see
			 *     https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1584#note_2150972079). The cross reference
			 *     holds the converted values, which do (YDBOcto#660);
			 *   - for a string key column: M orders the subscripts of its global numbers first ("3", "9", "10", "A"),
			 *     not as SQL orders the strings ('10', '3', '9', 'A'). The cross reference holds "#"_value, which sorts
			 *     as SQL does (YDBOcto#660).
			 */
			use_xref = ((aggr_column != key_column) || is_composite_key || IS_XREF_VALUE_TRANSFORMED(table, aggr_column)
				    || IS_COLUMN_STRING_TYPE(aggr_column));
			if (use_xref) {
				if (IS_KEY_COLUMN(aggr_column) && (NULL != get_keyword(aggr_column, OPTIONAL_GLOBAL))) {
					/* A key column with a GLOBAL keyword of its own (possible only in a table created
					 * before YDBOcto#1197, hence the assert) has no cross reference
					 * ("lp_generate_xref_keys()").
					 */
					assert(FALSE);
					return FALSE;
				}
				if (0 == get_column_global_key_prefix(aggr_column, table)) {
					/* The column's own GLOBAL lists no key column: one node holds the value of every row, and
					 * AIM cross references the subscripts of a node (it would stop with %YDBAIM-F-NOSUBS,
					 * YDBOcto#1191). A WHERE does not use such a cross reference either
					 * ("get_xref_advance_keys()").
					 */
					return FALSE;
				}
				if (NULL != get_keyword(aggr_column, OPTIONAL_EXTRACT)) {
					/* The value of an EXTRACT column is computed by an M expression, not stored in a global
					 * variable node that AIM could index (YDBOcto#1138). The WHERE path does not cross
					 * reference it either ("lp_generate_xref_keys()").
					 */
					return FALSE;
				}
			}
			if (use_xref && (is_composite_key || (aggr_column == key_column))) {
				/* The cross reference of a key column, or of a column of a table with several key columns
				 * (YDBOcto#660) */
				SqlOptionalKeyword *source_keyword;

				UNPACK_SQL_STATEMENT(source_keyword, table->source, keyword);
				if ('^' != source_keyword->v->v.value->v.string_literal[0]) {
					return FALSE; /* A table kept in a local variable has no such cross reference */
				}
				if (-1 == get_column_global_key_prefix(aggr_column, table)) {
					/* The column's GLOBAL lists the key columns in another order (e.g.
					 * "^R(keys(""b""),keys(""a""))" for PRIMARY KEY (a, b)): its cross reference does not hold
					 * the column's values for the rows of the table. A WHERE does not use it either
					 * ("get_xref_advance_keys()").
					 */
					return FALSE;
				}
				if (has_key_column_filter(pplan, aggr_column)) {
					return FALSE; /* The first or last value of the cross reference may not be a row (see above)
						       */
				}
			}
			if (use_xref) {
				/* Even though we have not looked at ALL aggregate function usages in this query, assume that
				 * YDBOcto#617 optimization is possible and allocate an xref plan for this column. If later we
				 * determine the optimization is not possible, we will just skip looking at this allocated plan
				 * and reclaim the allocated memory anyways at the end of the query so nothing is lost.
				 */
				SqlTableAlias *table_alias;
				UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
				af->extra_detail.lp_aggregate_function.octo617_xref_plan
				    = lp_generate_xref_plan(table, aggr_column, table_alias->unique_id);
			} else {
				if (NULL != get_keyword(key_column, OPTIONAL_END)) {
					/* It is not easy to support END with YDBOcto#617 optimization because it is a boolean
					 * condition and not a terminating value for the valid range. Therefore disable the
					 * optimization in this case.
					 */
					return FALSE;
				}
				if (NULL != key->fixed_to_value) {
					/* If key fixing optimization is enabled, then YDBOcto#617 optimization cannot be also
					 * enabled because the former implies a WHERE clause was specified and that does not work
					 * with YDBOcto#617.
					 */
					return FALSE;
				}
			}
			switch (aggr_column->data_type_struct.data_type) {
			case NUMERIC_TYPE:
			case STRING_TYPE:
			case INTEGER_TYPE:
			case DATE_TYPE:
			case TIME_TYPE:
			case TIMESTAMP_TYPE:
			case TIME_WITH_TIME_ZONE_TYPE:
			case TIMESTAMP_WITH_TIME_ZONE_TYPE:
				/* A date/time column of a READONLY table goes through its cross reference ("use_xref" above), which
				 * holds its values converted to Octo's internal form, as those of a READWRITE table are stored.
				 */
				break;
			case BOOLEAN_TYPE: /* not possible because this would have issued a ERR_MISTYPED_FUNCTION error */
			case NUL_TYPE:	   /* not possible because CREATE TABLE does not allow such a type for a column */
			default:
				assert(FALSE);
				break;
			}
			break;
		default:
			assert(FALSE);
			return FALSE;
			break;
		}
	}
	/* If we reached here, it means the YDBOcto#617 optimization is possible. */
	return TRUE;
}
