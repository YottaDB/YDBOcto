/****************************************************************
 *								*
 * Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	*
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
			if (1 != pplan->total_iter_keys) {
				/* Is a table with multiple key column (i.e. composite key).
				 * Cannot apply the YDBOcto#617 optimization for now.
				 * Will be enabled in a future commit once YottaDB/Util/YDBAIM#5 is fixed.
				 */
				return FALSE;
			}

			SqlKey	  *key;
			SqlColumn *aggr_column, *key_column;
			SqlTable  *table;
			aggr_column = column_alias->column->v.column;
			key = pplan->iterKeys[0];
			key_column = key->column;
			table = key->table;
			assert(NULL == af->extra_detail.lp_aggregate_function.octo617_xref_plan);
			if (aggr_column != key_column) {
				/* Aggregate function is used on a non-key column. Even though we have not looked at
				 * ALL aggregate function usages in this query, assume that YDBOcto#617 optimization
				 * is possible and allocate an xref plan for this non-key column. If later we determine
				 * the optimization is not possible, we will just skip looking at this allocated plan
				 * and reclaim the allocated memory anyways at the end of the query so nothing is lost.
				 */
				SqlTableAlias *table_alias;
				UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
				assert(NULL == af->extra_detail.lp_aggregate_function.octo617_xref_plan);
				af->extra_detail.lp_aggregate_function.octo617_xref_plan
				    = lp_generate_xref_plan(table, aggr_column, table_alias->unique_id);
			} else {
				if (NULL != get_keyword(key_column, OPTIONAL_STARTINCLUDE)) {
					/* It is easy to support START and ENDPOINT with YDBOcto#617 optimization but not so easy to
					 * support STARTINCLUDE. Since no user cares about this flag, just disable the optimization
					 * in case STARTINCLUDE is specified.
					 */
					return FALSE;
				}
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
			/* Disable optimization in case of a READONLY table and for date/time/boolean typed key column.
			 * See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1584#note_2150972079 for details.
			 */
			switch (aggr_column->data_type_struct.data_type) {
			case NUMERIC_TYPE:
			case STRING_TYPE:
			case INTEGER_TYPE:
				break;
			case DATE_TYPE:
			case TIME_TYPE:
			case TIMESTAMP_TYPE:
			case TIME_WITH_TIME_ZONE_TYPE:
			case TIMESTAMP_WITH_TIME_ZONE_TYPE:
				if (!table->readwrite) {
					return FALSE;
				}
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
