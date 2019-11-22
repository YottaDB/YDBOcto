/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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
LogicalPlan *generate_logical_plan(SqlStatement *stmt, int *plan_id) {
	SqlSelectStatement	*select_stmt;
	SqlTableAlias		*table_alias;
	LogicalPlan		*insert, *project, *select, *dst, *dst_key;
	LogicalPlan		*criteria, *where, *order_by, *select_options;
	LogicalPlan		*join_left, *join_right, *temp, *select_right;
	LogicalPlan		*start_join_condition, *t_join_condition;
	LogicalPlan		*keywords, *left;
	SqlJoin			*cur_join, *start_join;
	SqlColumnListAlias	*list;
	int			num_outer_joins;
	enum SqlJoinType 	cur_join_type;
	boolean_t		null_return_seen = FALSE;

	// Set operations should be handled in a different function
	if (set_operation_STATEMENT == stmt->type) {
		return lp_generate_set_logical_plan(stmt, plan_id);
	}

	UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
	UNPACK_SQL_STATEMENT(select_stmt, table_alias->table, select);
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);

	/* Compute `num_outer_joins` so it can be stored in the LP_WHERE logical plan for later use in `optimize_logical_plan.c` */
	cur_join = start_join;
	num_outer_joins = 0;
	do
	{
		cur_join_type = cur_join->type;
		if ((LEFT_JOIN == cur_join_type) || (RIGHT_JOIN == cur_join_type) || (FULL_JOIN == cur_join_type))
			num_outer_joins++;
		cur_join = cur_join->next;
	} while (cur_join != start_join);
	MALLOC_LP_2ARGS(insert, LP_INSERT);
	MALLOC_LP(project, insert->v.operand[0], LP_PROJECT);
	MALLOC_LP(select, project->v.operand[1], LP_SELECT);
	MALLOC_LP(criteria, select->v.operand[1], LP_CRITERIA);
	MALLOC_LP_2ARGS(criteria->v.operand[0], LP_KEYS);
	MALLOC_LP(select_options, criteria->v.operand[1], LP_SELECT_OPTIONS);
	MALLOC_LP(where, select_options->v.operand[0], LP_WHERE);
	LP_GENERATE_WHERE(select_stmt->where_expression, plan_id, stmt, where->v.operand[0], null_return_seen);
	insert->counter = plan_id;
	MALLOC_LP(dst, insert->v.operand[1], LP_OUTPUT);
	MALLOC_LP(dst_key, dst->v.operand[0], LP_KEY);
	OCTO_CMALLOC_STRUCT(dst_key->v.key, SqlKey);
	memset(dst_key->v.key, 0, sizeof(SqlKey));
	dst_key->v.key->unique_id = get_plan_unique_number(insert);
	if (NULL != select_stmt->order_expression) {
		SqlColumnListAlias	*cur_cla, *start_cla;

		MALLOC_LP(order_by, dst->v.operand[1], LP_COLUMN_LIST);
		UNPACK_SQL_STATEMENT(list, select_stmt->order_expression, column_list_alias);
		cur_cla = start_cla = list;
		do {
			SqlColumnListAlias	*save_next;
			SqlOptionalKeyword	*keyword;

			save_next = cur_cla->next;
			cur_cla->next = cur_cla;	/* set "next" to self so below call processes only one column instead of
							 * multiple columns in table corresponding to the desired column.
							 */
			order_by->v.operand[0] = lp_column_list_to_lp(cur_cla, plan_id);
			cur_cla->next = save_next;
			if (NULL != cur_cla->keywords)
			{
				UNPACK_SQL_STATEMENT(keyword, cur_cla->keywords, keyword);
				assert(keyword->next == keyword);
				assert(keyword->prev == keyword);
			} else
				keyword = NULL;
			order_by->extra_detail = (NULL != keyword) ? keyword->keyword : OPTIONAL_ASC;	/* ASC is default */
			cur_cla = cur_cla->next;
			if (cur_cla != start_cla) {
				MALLOC_LP_2ARGS(order_by->v.operand[1], LP_COLUMN_LIST);
				order_by = order_by->v.operand[1];
			}
		} while (cur_cla != start_cla);
	}
	/// TODO: we should look at the columns to decide which values
	//   are keys, and if none, create a rowId as part of the advance
	dst_key->v.key->type = LP_KEY_ADVANCE;
	join_right = NULL;
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);
	cur_join = start_join;
	cur_join_type = NO_JOIN;
	start_join_condition = NULL;
	do {
		SqlStatement	*sql_stmt;

		assert((NO_JOIN == cur_join->type) || (INNER_JOIN == cur_join->type)
			|| (LEFT_JOIN == cur_join->type) || (RIGHT_JOIN == cur_join->type) || (FULL_JOIN == cur_join->type)
			|| (CROSS_JOIN == cur_join->type) || (NATURAL_JOIN == cur_join->type));
		if (NULL == join_right) {
			MALLOC_LP(join_right, select->v.operand[0], LP_TABLE_JOIN);
		} else {
			MALLOC_LP_2ARGS(join_right->v.operand[1], LP_TABLE_JOIN);
			join_right = join_right->v.operand[1];
		}
		assert(set_operation_STATEMENT != stmt->type);	/* else would have returned at beginning of this function */
		sql_stmt = cur_join->value;
		if ((table_alias_STATEMENT == sql_stmt->type) && (table_STATEMENT == sql_stmt->v.table_alias->table->type))
		{
			MALLOC_LP(join_left, join_right->v.operand[0], LP_TABLE);
			join_left->v.table_alias = sql_stmt->v.table_alias;
		} else
		{
			join_left = generate_logical_plan(sql_stmt, plan_id);
			if (NULL == join_left)
				return NULL;
			join_right->v.operand[0] = join_left;
		}
		join_right->join_on_condition = NULL;
		cur_join_type = cur_join->type;
		join_right->extra_detail = cur_join_type;
		if (cur_join->condition)
		{
			MALLOC_LP_2ARGS(t_join_condition, LP_WHERE);
			LP_GENERATE_WHERE(cur_join->condition, plan_id, stmt, t_join_condition->v.operand[0], null_return_seen);
			if (num_outer_joins)
				join_right->join_on_condition = t_join_condition;
			else
			{	/* No OUTER JOINs. We can safely add the ON clause in the join condition to the
				 * WHERE clause without risk of correctness issues.
				 */
				start_join_condition = lp_join_where(start_join_condition, t_join_condition);
			}
		}
		cur_join = cur_join->next;
	} while (cur_join != start_join);

	if (NULL == select_stmt->select_list->v.column_list) {
		temp = lp_table_join_to_column_list(select->v.operand[0], plan_id);
		// Append the columns from any subqueries to the list
		select_right = temp;
		while ((NULL != select_right) && (NULL != select_right->v.operand[1]))
			select_right = select_right->v.operand[1];
	        join_right = select->v.operand[0];
		while (NULL != join_right) {
			join_left = join_right->v.operand[0];
			if (join_left->type == LP_PROJECT) {
				if (NULL == select_right) {
				        temp = select_right = lp_copy_plan(join_left->v.operand[0]);
				} else {
					select_right->v.operand[1] = lp_copy_plan(join_left->v.operand[0]);
					while (NULL != select_right->v.operand[1])
						select_right = select_right->v.operand[1];
				}
			}
			join_right = join_right->v.operand[1];
		}
	} else {
		UNPACK_SQL_STATEMENT(list, select_stmt->select_list, column_list_alias);
		temp = lp_column_list_to_lp(list, plan_id);
	}

	// Ensure that any added conditions as a result of a join are added to the WHERE
	// before we go through and replace derived table references
	project->v.operand[0] = temp;
	if (NULL != start_join_condition)
	{
		where = lp_join_where(start_join_condition, where);
		select_options->v.operand[0] = where;
	}
	where->v.operand[1] = NULL;
	where->extra_detail = num_outer_joins;	/* used later in "optimize_logical_plan" */

	// Handle factoring in derived columns
	left = select->v.operand[0];
	cur_join = start_join;
	while (NULL != left) {
		LogicalPlan	*new_plan;

		new_plan = left->v.operand[0];
		assert((LP_INSERT == new_plan->type) || (LP_SET_OPERATION == new_plan->type) || (LP_TABLE == new_plan->type));
		if (LP_TABLE != new_plan->type)
		{
			SqlStatement	*sql_stmt;
			LogicalPlan	*cur_lp_key;

			sql_stmt = cur_join->value;
			sql_stmt = drill_to_table_alias(sql_stmt);
			UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
			cur_lp_key = lp_get_output_key(new_plan);
			lp_replace_derived_table_references(insert, table_alias, cur_lp_key->v.key);
		}
		left = left->v.operand[1];
		cur_join = cur_join->next;
	}

	MALLOC_LP(keywords, select_options->v.operand[1], LP_KEYWORDS);
	UNPACK_SQL_STATEMENT(keywords->v.keywords, select_stmt->optional_words, keyword);

	// At this point, we need to populate keys
	//  Before we can do that, we need to resolve the JOINs, which are
	//  an area where we definitely want to look at optimization
	//  therefore, we leave the plan in a semi-valid state
	return (null_return_seen ? NULL : insert);
}
