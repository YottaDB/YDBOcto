/****************************************************************
 *								*
 * Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	*
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
			LogicalPlan *lp_key, *lp_column_list, *lp_where, *lp_column_alias;

			assert(NULL != cur_keys);
			assert(NULL != cur_order_by);
			GET_LP(lp_key, cur_keys, 0, LP_KEY);

			SqlKey *key;
			key = lp_key->v.lp_key.key;

			if ((LP_KEY_FIX != key->type) || !key->is_cross_reference_key) {
				assert(!key->emit_desc_order); /* assert this is initialized to FALSE at key allocation time */
				assert((NULL != key->table) || (NULL == key->column));
				assert((NULL == key->table) || (NULL != key->column));
				if (NULL == key->table) {
					/* The key does not correspond to a SqlTable. Cannot optimize. */
					assert(0 == loop);
					break;
				}

				GET_LP(lp_column_list, cur_order_by, 0, LP_COLUMN_LIST);
				GET_LP(lp_where, lp_column_list, 0, LP_WHERE);
				lp_column_alias = lp_where->v.lp_default.operand[0];
				if (LP_COLUMN_ALIAS != lp_column_alias->type) {
					/* The ORDER BY is not on a column alias. Cannot optimize. */
					assert(0 == loop);
					break;
				}

				SqlColumnAlias *column_alias;
				column_alias = lp_column_alias->v.lp_column_alias.column_alias;

				SqlStatement *column;
				column = column_alias->column;
				if (column_STATEMENT != column->type) {
					assert(0 == loop);
					break; /* The column alias is not on a SqlColumn. Cannot optimize. */
				}
				if (key->unique_id != column_alias->table_alias_stmt->v.table_alias->unique_id) {
					assert(0 == loop);
					break; /* The ORDER BY and KEY column correspond to different tables. Cannot optimize. */
				}

				SqlColumn *sql_column;
				sql_column = column->v.column;
				if (key->column != sql_column) {
					assert(0 == loop);
					break; /* KEY column is not same as ORDER BY column. Cannot optimize. */
				}

				/* ORDER BY and FOR loop order are guaranteed to be identical only for INTEGER, NUMERIC and
				 * BOOLEAN types. Not for STRING type (for more details, see
				 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/959#note_1612645682).
				 * So skip optimization for the STRING case. In the future, types like DATE/TIME etc.
				 * would need to be examined on a case-by-case basis and either supported or not in the
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

				boolean_t ok_to_optimize;
				switch (key->fixed_to_value_type) {
				case LP_BOOLEAN_EQUALS:
				case LP_BOOLEAN_IS:
					ok_to_optimize = TRUE;
					break;
				case LP_BOOLEAN_IN:
					/* This is a xref key that is fixed to a specific value. Skip this key column for the
					 * purposes of the ORDER BY optimization as long as it is fixed to ONE value. If this is
					 * fixed to a LIST of values (i.e. IN operator) then we cannot optimize ORDER BY as the
					 * processing order would be rows sorted on the cross referenced non-key column value which
					 * is not the same as the ORDER BY order (within one non-key column value, all rows will be
					 * sorted in primary key column order, but the primary key column values corresponding to
					 * the first non-key column value are not guaranteed to be in sorted order compared to the
					 * primary key column values corresponding to the second (or later) non-key column values).
					 */
					ok_to_optimize = FALSE;
					break;
				case LP_BOOLEAN_LESS_THAN:
				case LP_BOOLEAN_GREATER_THAN:
				case LP_BOOLEAN_LESS_THAN_OR_EQUALS:
				case LP_BOOLEAN_GREATER_THAN_OR_EQUALS:
					/* This is an xref key that is used for a range of key values.
					 * For the same reasons as described in the previous comment block (LIST of values)
					 * disable the ORDER BY optimization in this case.
					 */
					ok_to_optimize = FALSE;
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
