/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
	SqlSelectStatement *select_stmt;
	LogicalPlan *insert, *project, *column_list, *select, *dst, *dst_key;
	LogicalPlan *criteria, *table, *keys, *where, *order_by, *select_options;
	LogicalPlan *join_left, *join_right, *temp, *select_right, *select_left;
	LogicalPlan *start_join_condition = NULL, *cur_join_condition = NULL, *t_join_condition;
	LogicalPlan *keywords, *left;
	SqlJoin *cur_join, *start_join;
	SqlColumnListAlias *list;
	SqlTableAlias *table_alias;

	UNPACK_SQL_STATEMENT(select_stmt, stmt, select);
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);

	// If this is a SELECT involved with a SET operation, we want to perform
	//  that operation on the output keys of this plan and the next one, in order,
	//  so we should generate a placeholder plan which does a LP_KEY_<set operation> on
	//  those keys
	if(select_stmt->set_operation != NULL) {
		return lp_generate_set_logical_plan(stmt, plan_id);
	}
	//MALLOC_LP(start_join_condition, LP_WHERE);
	//start_join_condition->v.operand[0] = generate_lp_where(start_join->condition, plan_id);

	MALLOC_LP(insert, LP_INSERT);
	project = MALLOC_LP(insert->v.operand[0], LP_PROJECT);
	select = MALLOC_LP(project->v.operand[1], LP_SELECT);
	criteria = MALLOC_LP(select->v.operand[1], LP_CRITERIA);
	keys = MALLOC_LP(criteria->v.operand[0], LP_KEYS);
	select_options = MALLOC_LP(criteria->v.operand[1], LP_SELECT_OPTIONS);
	where = MALLOC_LP(select_options->v.operand[0], LP_WHERE);
	where->v.operand[0] = lp_generate_where(select_stmt->where_expression, plan_id);
	insert->counter = plan_id;
	dst = MALLOC_LP(insert->v.operand[1], LP_OUTPUT);
	dst_key = MALLOC_LP(dst->v.operand[0], LP_KEY);
	dst_key->v.key = (SqlKey*)octo_cmalloc(memory_chunks, sizeof(SqlKey));
	memset(dst_key->v.key, 0, sizeof(SqlKey));
	dst_key->v.key->random_id = get_plan_unique_number(insert);
	if(select_stmt->order_expression != NULL) {
		order_by = MALLOC_LP(dst->v.operand[1], LP_COLUMN_LIST);
		UNPACK_SQL_STATEMENT(list, select_stmt->order_expression, column_list_alias);
		order_by->v.operand[0] = lp_column_list_to_lp(list, plan_id);
	}
	/// TODO: we should look at the columns to decide which values
	//   are keys, and if none, create a rowId as part of the advance
	dst_key->v.key->type = LP_KEY_ADVANCE;
	//join_right = table = MALLOC_LP(select->v.operand[0], LP_TABLE_JOIN);
	join_right = NULL;
	cur_join = start_join;
	do {
		if(cur_join->type != INNER_JOIN
		   && cur_join->type != CROSS_JOIN && cur_join->type != NO_JOIN
		   && cur_join->type != NATURAL_JOIN) {
			FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "OUTER JOIN");
		}
		if(join_right == NULL) {
			join_right =  MALLOC_LP(select->v.operand[0], LP_TABLE_JOIN);
		}
		else {
			MALLOC_LP(join_right->v.operand[1], LP_TABLE_JOIN);
			join_right = join_right->v.operand[1];
		}
		UNPACK_SQL_STATEMENT(table_alias, cur_join->value, table_alias);
		if(table_alias->table->type == table_STATEMENT) {
			join_left = MALLOC_LP(join_right->v.operand[0], LP_TABLE);
			UNPACK_SQL_STATEMENT(join_left->v.table_alias, cur_join->value, table_alias);
		} else {
			join_left = generate_logical_plan(table_alias->table, plan_id);
			join_right->v.operand[0] = join_left;
		}
		if(cur_join->condition) {
			MALLOC_LP(t_join_condition, LP_WHERE);
			t_join_condition->v.operand[0] = lp_generate_where(cur_join->condition, plan_id);
			start_join_condition = lp_join_where(start_join_condition, t_join_condition);
		}
		cur_join = cur_join->next;
	} while(cur_join != start_join);

	if(select_stmt->select_list->v.column_list == NULL) {
		temp = lp_table_join_to_column_list(select->v.operand[0], plan_id);
		// Append the columns from any subqueries to the list
		select_right = temp;
		while(select_right != NULL && select_right->v.operand[1] != NULL) {
			select_right = select_right->v.operand[1];
		}
	        join_right = select->v.operand[0];
		while(join_right != NULL) {
			join_left = join_right->v.operand[0];
			if(join_left->type == LP_PROJECT) {
				if(select_right == NULL) {
				        temp = select_right = lp_copy_plan(join_left->v.operand[0]);
				} else {
					select_right->v.operand[1] = lp_copy_plan(join_left->v.operand[0]);
					while(select_right->v.operand[1] != NULL) {
						select_right = select_right->v.operand[1];
					}
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
	if(start_join_condition) {
		where = lp_join_where(where, start_join_condition);
		select_options->v.operand[0] = where;
	}
	where->v.operand[1] = NULL;

	// Handle factoring in derived columns
	left = select->v.operand[0];
	cur_join = start_join;
	while(left != NULL) {
		/// TODO: welcome back, you need to somehow make the table aliases from
		//   the loop where we fill in the TABLE_JOIN fields come down here
		//   basically move the call to lp_replace_derived_table_references(insert, join_left, table_alias); here
		//   we need it here so we can replace the PROJECT list when needed
		// We need to call this function on the WHERE and PROJECT fields
		if(left->v.operand[0]->type == LP_INSERT) {
			UNPACK_SQL_STATEMENT(table_alias, cur_join->value, table_alias);
			lp_replace_derived_table_references(insert, left->v.operand[0], table_alias);
		}
		left = left->v.operand[1];
		cur_join = cur_join->next;
	}



	keywords = MALLOC_LP(select_options->v.operand[1], LP_KEYWORDS);
	UNPACK_SQL_STATEMENT(keywords->v.keywords, select_stmt->optional_words, keyword);

	// At this point, we need to populate keys
	//  Before we can do that, we need to resolve the JOINs, which are
	//  an area where we definetely want to look at optimization
	//  therefore, we leave the plan in a semi-valid state
	return insert;
}
