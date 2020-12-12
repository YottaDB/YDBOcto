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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/**
 * Generates a LP for the given column list, with a root element of
 *  LP_COLUMN_LIST and child elements of LP_WHERE and LP_COLUMN_LIST
 */
LogicalPlan *generate_logical_plan(SqlStatement *stmt) {
	SqlSelectStatement *select_stmt;
	SqlTableAlias *	    table_alias;
	LogicalPlan *	    select_query, *project, *select, *dst, *dst_key;
	LogicalPlan *	    criteria, *where, *order_by;
	LogicalPlan *	    select_options, *select_more_options, *aggregate_options;
	LogicalPlan *	    join_right, *temp;
	LogicalPlan *	    start_join_condition, *t_join_condition;
	LogicalPlan *	    keywords, *left;
	SqlJoin *	    cur_join, *start_join;
	SqlColumnListAlias *list;
	int		    num_outer_joins;
	enum SqlJoinType    cur_join_type;
	boolean_t	    error_encountered;

	error_encountered = FALSE;
	// Set operations should be handled in a different function
	if (set_operation_STATEMENT == stmt->type) {
		return lp_generate_set_logical_plan(stmt);
	} else if (insert_STATEMENT == stmt->type) {
		SqlInsertStatement *insert;
		LogicalPlan *	    lp_insert_into, *lp_insert_into_options;
		LogicalPlan *	    lp_table;
		LogicalPlan *	    lp_select_query;

		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		MALLOC_LP_2ARGS(lp_insert_into, LP_INSERT_INTO);
		MALLOC_LP(lp_table, lp_insert_into->v.lp_default.operand[0], LP_TABLE);
		lp_table->v.lp_table.table_alias = insert->dst_table_alias;
		MALLOC_LP(lp_insert_into_options, lp_insert_into->v.lp_default.operand[1], LP_INSERT_INTO_OPTIONS);
		lp_select_query = generate_logical_plan(insert->src_table_alias_stmt);
		if (NULL == lp_select_query) {
			return NULL;
		}
		lp_insert_into_options->v.lp_default.operand[1] = lp_select_query;
		if (NULL != insert->columns) {
			SqlColumnList *start_cl;

			UNPACK_SQL_STATEMENT(start_cl, insert->columns, column_list);
			error_encountered
			    |= lp_generate_column_list(&lp_insert_into_options->v.lp_default.operand[0], NULL, start_cl);
		} else {
			lp_insert_into_options->v.lp_default.operand[0] = NULL;
		}
		return (error_encountered ? NULL : lp_insert_into);
	}
	UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
	if (table_value_STATEMENT == table_alias->table->type) {
		select_query = lp_generate_table_value(stmt, &error_encountered);
		return (error_encountered ? NULL : select_query);
	}
	UNPACK_SQL_STATEMENT(select_stmt, table_alias->table, select);
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);

	cur_join = start_join;
	/* Compute `num_outer_joins` so it can be stored in the LP_WHERE logical plan for later use in `optimize_logical_plan.c` */
	num_outer_joins = 0;
	do {
		cur_join_type = cur_join->type;
		if ((LEFT_JOIN == cur_join_type) || (RIGHT_JOIN == cur_join_type) || (FULL_JOIN == cur_join_type)) {
			num_outer_joins++;
		}
		cur_join = cur_join->next;
	} while (cur_join != start_join);
	MALLOC_LP_2ARGS(select_query, LP_SELECT_QUERY);
	MALLOC_LP(project, select_query->v.lp_default.operand[0], LP_PROJECT);
	MALLOC_LP(select, project->v.lp_default.operand[1], LP_SELECT);
	MALLOC_LP(criteria, select->v.lp_default.operand[1], LP_CRITERIA);
	MALLOC_LP_2ARGS(criteria->v.lp_default.operand[0], LP_KEYS);
	MALLOC_LP(select_options, criteria->v.lp_default.operand[1], LP_SELECT_OPTIONS);
	MALLOC_LP(where, select_options->v.lp_default.operand[0], LP_WHERE);
	LP_GENERATE_WHERE(select_stmt->where_expression, stmt, where->v.lp_default.operand[0], error_encountered);
	MALLOC_LP(dst, select_query->v.lp_default.operand[1], LP_OUTPUT);
	MALLOC_LP(dst_key, dst->v.lp_default.operand[0], LP_KEY);
	OCTO_CMALLOC_STRUCT(dst_key->v.lp_key.key, SqlKey);
	dst_key->v.lp_key.key->unique_id = get_new_plan_unique_id();
	select_query->extra_detail.lp_select_query.root_table_alias = table_alias;
	/// TODO: we should look at the columns to decide which values
	//   are keys, and if none, create a rowId as part of the advance
	dst_key->v.lp_key.key->type = LP_KEY_ADVANCE;
	join_right = NULL;
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);
	cur_join = start_join;
	start_join_condition = NULL;
	do {
		SqlStatement *sql_stmt;
		boolean_t     need_generate_logical_plan;

		assert((NO_JOIN == cur_join->type) || (INNER_JOIN == cur_join->type) || (LEFT_JOIN == cur_join->type)
		       || (RIGHT_JOIN == cur_join->type) || (FULL_JOIN == cur_join->type) || (CROSS_JOIN == cur_join->type)
		       || (NATURAL_JOIN == cur_join->type));
		if (NULL == join_right) {
			MALLOC_LP(join_right, select->v.lp_default.operand[0], LP_TABLE_JOIN);
		} else {
			MALLOC_LP_2ARGS(join_right->v.lp_default.operand[1], LP_TABLE_JOIN);
			join_right = join_right->v.lp_default.operand[1];
		}
		assert(set_operation_STATEMENT != stmt->type); /* else would have returned at beginning of this function */
		sql_stmt = cur_join->value;
		need_generate_logical_plan = TRUE;
		if (table_alias_STATEMENT == sql_stmt->type) {
			SqlStatementType table_type;
			SqlTableAlias *	 table_alias;

			table_alias = sql_stmt->v.table_alias;
			table_type = table_alias->table->type;
			if (select_STATEMENT != table_type) {
				if (create_table_STATEMENT == table_type) {
					LogicalPlan *lp_table;

					MALLOC_LP(lp_table, join_right->v.lp_default.operand[0], LP_TABLE);
					lp_table->v.lp_table.table_alias = table_alias;
				} else {
					assert(table_value_STATEMENT == table_type);
					join_right->v.lp_default.operand[0] = lp_generate_table_value(sql_stmt, &error_encountered);
				}
				need_generate_logical_plan = FALSE;
			}
		}
		if (need_generate_logical_plan) {
			LogicalPlan *join_left;

			join_left = generate_logical_plan(sql_stmt);
			if (NULL == join_left) {
				return NULL;
			}
			join_right->v.lp_default.operand[0] = join_left;
		}
		assert(NULL == join_right->extra_detail.lp_table_join.join_on_condition);
		cur_join_type = cur_join->type;
		join_right->extra_detail.lp_table_join.cur_join_type = cur_join_type;
		if (cur_join->condition) {
			MALLOC_LP_2ARGS(t_join_condition, LP_WHERE);
			LP_GENERATE_WHERE(cur_join->condition, stmt, t_join_condition->v.lp_default.operand[0], error_encountered);
			join_right->extra_detail.lp_table_join.join_on_condition = t_join_condition;
		}
		cur_join = cur_join->next;
	} while (cur_join != start_join);

	// Add SELECT column list to logical plan
	assert(NULL != select_stmt->select_list->v.column_list);
	UNPACK_SQL_STATEMENT(list, select_stmt->select_list, column_list_alias);
	temp = lp_column_list_to_lp(list, &error_encountered);
	project->v.lp_default.operand[0] = temp;

	/* Now that SELECT column list is done, add ORDER BY column list to logical plan.
	 * Need to do this in this order so any references in ORDER BY to a COLUMN NUM are automatically translated to
	 * the corresponding logical plan already generated for that COLUMN NUM in the "lp_column_list_to_lp" call above
	 * for the SELECT column list. Not doing so could cause issues if a sub-query is part of that column (the sub-query
	 * would have different unique_id values generated one for the SELECT column list and one for the ORDER BY COLUMN NUM
	 * reference which would cause incorrect results).
	 */
	if (NULL != select_stmt->order_by_expression) {
		SqlColumnListAlias *cur_cla, *start_cla;

		MALLOC_LP(order_by, dst->v.lp_default.operand[1], LP_ORDER_BY);
		UNPACK_SQL_STATEMENT(list, select_stmt->order_by_expression, column_list_alias);
		cur_cla = start_cla = list;
		do {
			SqlColumnListAlias *save_next;
			SqlOptionalKeyword *keyword;

			if (cur_cla->tbl_and_col_id.unique_id) {
				LogicalPlan *column_list, *select_column_list;

				/* This is an ORDER BY COLUMN NUM usage. Get the Nth LP_COLUMN_LIST from the SELECT column list
				 * and connect that to this LP_ORDER_BY plan.
				 */
				MALLOC_LP_2ARGS(column_list, LP_COLUMN_LIST);
				assert(cur_cla->tbl_and_col_id.column_number);
				select_column_list
				    = lp_get_col_num_n_in_select_column_list(temp, cur_cla->tbl_and_col_id.column_number);
				assert(LP_COLUMN_LIST == select_column_list->type);
				column_list->v.lp_default.operand[0] = select_column_list->v.lp_default.operand[0];
				order_by->v.lp_default.operand[0] = column_list;
				order_by->extra_detail.lp_order_by.order_by_column_num = TRUE;
			} else {
				save_next = cur_cla->next;
				cur_cla->next = cur_cla; /* set "next" to self so below call processes only one column instead of
							  * multiple columns in table corresponding to the desired column.
							  */
				order_by->v.lp_default.operand[0] = lp_column_list_to_lp(cur_cla, &error_encountered);
				cur_cla->next = save_next;
				order_by->extra_detail.lp_order_by.order_by_column_num = FALSE;
			}
			if (NULL != cur_cla->keywords) {
				UNPACK_SQL_STATEMENT(keyword, cur_cla->keywords, keyword);
				assert(keyword->next == keyword);
				assert(keyword->prev == keyword);
			} else
				keyword = NULL;
			order_by->extra_detail.lp_order_by.direction = (NULL != keyword) ? keyword->keyword : OPTIONAL_ASC;
			/* ASCENDING order is default direction for ORDER BY */
			cur_cla = cur_cla->next;
			if (cur_cla != start_cla) {
				MALLOC_LP_2ARGS(order_by->v.lp_default.operand[1], LP_ORDER_BY);
				order_by = order_by->v.lp_default.operand[1];
			}
		} while (cur_cla != start_cla);
	}

	// Ensure that any added conditions as a result of a join are added to the WHERE
	// before we go through and replace derived table references
	if (NULL != start_join_condition) {
		where = lp_join_where(start_join_condition, where);
		select_options->v.lp_default.operand[0] = where;
	}
	where->v.lp_default.operand[1] = NULL;
	where->extra_detail.lp_where.num_outer_joins = num_outer_joins; /* used later in "optimize_logical_plan" */

	// Add GROUP BY and HAVING to logical plan
	MALLOC_LP(select_more_options, select_options->v.lp_default.operand[1], LP_SELECT_MORE_OPTIONS);
	if ((NULL != select_stmt->group_by_expression) || (NULL != select_stmt->having_expression)) {
		MALLOC_LP(aggregate_options, select_more_options->v.lp_default.operand[0], LP_AGGREGATE_OPTIONS);
		if (NULL != select_stmt->group_by_expression) {
			LogicalPlan *group_by;

			MALLOC_LP(group_by, aggregate_options->v.lp_default.operand[0], LP_GROUP_BY);
			UNPACK_SQL_STATEMENT(list, select_stmt->group_by_expression, column_list_alias);
			group_by->v.lp_default.operand[0] = lp_column_list_to_lp(list, &error_encountered);
		}
		if (NULL != select_stmt->having_expression) {
			LogicalPlan *having;

			MALLOC_LP(having, aggregate_options->v.lp_default.operand[1], LP_HAVING);
			MALLOC_LP(where, having->v.lp_default.operand[0], LP_WHERE);
			LP_GENERATE_WHERE(select_stmt->having_expression, stmt, where->v.lp_default.operand[0], error_encountered);
		}
	}
	// Handle factoring in derived columns
	left = select->v.lp_default.operand[0];
	cur_join = start_join;
	while (NULL != left) {
		LogicalPlan *new_plan;

		new_plan = left->v.lp_default.operand[0];
		assert((LP_SELECT_QUERY == new_plan->type) || (LP_SET_OPERATION == new_plan->type) || (LP_TABLE == new_plan->type)
		       || (LP_TABLE_VALUE == new_plan->type));
		if (LP_TABLE != new_plan->type) {
			SqlStatement *sql_stmt;
			LogicalPlan * cur_lp_key;

			sql_stmt = cur_join->value;
			sql_stmt = drill_to_table_alias(sql_stmt);
			UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
			cur_lp_key = lp_get_output_key(new_plan);
			lp_replace_derived_table_references(select_query, table_alias, cur_lp_key->v.lp_key.key);
			/* Check if any VALUES clause specification optimization happened. If so, do derived table check/replace
			 * for the pre-optimized "value" too (which is now stored in the "alternate_value" field).
			 * See comment in "octo_types.h" before "alternate_value" member in the "SqlJoin" structure for details.
			 */
			if (NULL != cur_join->alternate_value) {
				assert(LP_TABLE_VALUE == new_plan->type);
				sql_stmt = cur_join->alternate_value;
				sql_stmt = drill_to_table_alias(sql_stmt);
				UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
				lp_replace_derived_table_references(select_query, table_alias, cur_lp_key->v.lp_key.key);
			}
		}
		left = left->v.lp_default.operand[1];
		cur_join = cur_join->next;
	}

	MALLOC_LP(keywords, select_more_options->v.lp_default.operand[1], LP_KEYWORDS);
	UNPACK_SQL_STATEMENT(keywords->v.lp_keywords.keywords, select_stmt->optional_words, keyword);

	// At this point, we need to populate keys
	//  Before we can do that, we need to resolve the JOINs, which are
	//  an area where we definitely want to look at optimization
	//  therefore, we leave the plan in a semi-valid state

	/* Examine "error_encountered" variable to see if any errors were encountered inside LP_GENERATE_WHERE.
	 * If so, propagate this error back to caller so it can stop logical plan stage (i.e. not proceed to physical plan).
	 */
	return (error_encountered ? NULL : select_query);
}
