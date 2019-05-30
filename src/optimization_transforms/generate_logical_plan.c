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
	LogicalPlan *insert, *project, *select, *dst, *dst_key;
	LogicalPlan *criteria, *where, *order_by, *select_options;
	LogicalPlan *join_left, *join_right, *temp, *select_right;
	LogicalPlan *start_join_condition = NULL, *t_join_condition;
	LogicalPlan *keywords, *left, *t_left, *t_right;
	SqlStatement *table_alias_stmt1, *table_alias_stmt2, *top_set_opr_stmt, *t, *old_set_opr_stmt;
	SqlSetOperation *top_set_opr, *right_set_opr;
	SqlJoin *cur_join, *start_join;
	SqlJoin *cur_join2, *start_join2;
	SqlColumnListAlias *list;
	SqlTableAlias *table_alias, *table_alias_t1, *table_alias_t2;
	int removed_joins, new_id;
	enum SqlJoinType cur_join_type;

	// Set operations should be handled in a different function
	if(stmt->type == set_operation_STATEMENT) {
		return lp_generate_set_logical_plan(stmt, plan_id);
	}

	UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
	UNPACK_SQL_STATEMENT(select_stmt, table_alias->table, select);
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);

	// Before we get too far, scan the joins for OUTER JOINs and handle them
	// Otherwise we'll malloc things that never get used and waste time
	cur_join = start_join;
	do {
		if(cur_join->type == LEFT_JOIN || cur_join->type == RIGHT_JOIN || cur_join->type == FULL_JOIN) {
			// We want to generate a new SQL statement which has this outer join replaced
			// by a INNER JOIN, but we later need to make sure we remove the outer join when
			// creating statements without the second table; when we copy to stmt_t1, which is
			// what we eventually generate a logical plan for, mark this join as an inner join
			// but change it back right after so we can find the outer join we are removing later
			cur_join_type = cur_join->type;
			cur_join->type = INNER_JOIN;
			table_alias_stmt1 = copy_sql_statement(stmt);
			cur_join->type = cur_join_type;
			// We have to join these as a SqlBinaryStatement because we can't put in LogicalPlans just yet
			// LEFT = (t1 INTERSECT t2) UNION (t1 EXCEPT (t1 INTERSECT t2))
			//
			// Construct the boolean that will be used in the CROSS JOIN statements
			//
			// t1 INTERSECT t2 is replaced by a INNER JOIN with the condition
			// UNION ALL
			// ((the SELECT statement with the select list restricted to the table on the left)
			//	EXCEPT ALL
			//	(the above statement, with all other tables replaced by a null value)
			UNPACK_SQL_STATEMENT(table_alias_t1, table_alias_stmt1, table_alias);
			UNPACK_SQL_STATEMENT(select_stmt, table_alias_t1->table, select);

			// Construct the top level UNION
			SQL_STATEMENT(select_stmt->set_operation, set_operation_STATEMENT);
			top_set_opr_stmt = select_stmt->set_operation;
			MALLOC_STATEMENT(select_stmt->set_operation, set_operation, SqlSetOperation);
			UNPACK_SQL_STATEMENT(top_set_opr, select_stmt->set_operation, set_operation);
			top_set_opr->type = SET_UNION_ALL;
			top_set_opr->operand[0] = table_alias_stmt1;

			// Construct the SELECT statement with the right hand table values replaced
			// by a null a value; remove the right hand table from the JOIN
			// We gotta construct a SELECT where we select all the keys from each table
			// involved in the JOIN!
			right_set_opr = NULL;
			if(cur_join->type == LEFT_JOIN || cur_join->type == FULL_JOIN) {
				table_alias_stmt2 = copy_sql_statement(stmt);
				UNPACK_SQL_STATEMENT(table_alias_t2, table_alias_stmt2, table_alias);
				UNPACK_SQL_STATEMENT(select_stmt, table_alias_t2->table, select);
				// Make sure all tables have new keys
				UNPACK_SQL_STATEMENT(start_join2, select_stmt->table_list, join);
				cur_join2 = start_join2;
				removed_joins = 0;
				do {
					if(cur_join2->type == cur_join->type && removed_joins == 0) {
						removed_joins++;
						cur_join2->prev->next = cur_join2->next;
						cur_join2->next->prev = cur_join2->prev;
					} else {
						UNPACK_SQL_STATEMENT(table_alias, cur_join2->value, table_alias);
						new_id = (*plan_id)++;
						update_table_references(table_alias_stmt2, table_alias->unique_id, new_id);
						table_alias->unique_id = new_id;
					}
					cur_join2 = cur_join2->next;
				} while(cur_join2 != start_join2);
				replace_table_references(table_alias_stmt2, cur_join->value);

				// Setup the SET operations
				SQL_STATEMENT(select_stmt->set_operation, set_operation_STATEMENT);
				MALLOC_STATEMENT(select_stmt->set_operation, set_operation, SqlSetOperation);
				UNPACK_SQL_STATEMENT(right_set_opr, select_stmt->set_operation, set_operation);
				right_set_opr->type = SET_EXCEPT_ALL;
				right_set_opr->operand[0] = table_alias_stmt2;
				top_set_opr->operand[1] = select_stmt->set_operation;

				table_alias_stmt2 = copy_sql_statement(table_alias_stmt1);
				UNPACK_SQL_STATEMENT(table_alias_t2, table_alias_stmt2, table_alias);
				UNPACK_SQL_STATEMENT(select_stmt, table_alias_t2->table, select);

				// Make sure all tables have new keys
				UNPACK_SQL_STATEMENT(start_join2, select_stmt->table_list, join);
				// Before assigning new keys, make the right table return null values
				replace_table_references(select_stmt->select_list, cur_join->value);
				cur_join2 = start_join2;
				do {
					UNPACK_SQL_STATEMENT(table_alias, cur_join2->value, table_alias);
					new_id = (*plan_id)++;
					update_table_references(table_alias_stmt2, table_alias->unique_id, new_id);
					cur_join2 = cur_join2->next;
				} while(cur_join2 != start_join2);
				right_set_opr->operand[1] = table_alias_stmt2;
				select_stmt->set_operation = NULL;
			}

			if(cur_join->type == RIGHT_JOIN || cur_join->type == FULL_JOIN) {
				table_alias_stmt2 = copy_sql_statement(stmt);
				// If we have a right_set_opr, we need to make a set operation for the second statement
				// and do a UNION with the new statement, then the new statement does the same thing
				if(right_set_opr != NULL) {
					old_set_opr_stmt = top_set_opr->operand[1];
					SQL_STATEMENT(t, set_operation_STATEMENT);
					MALLOC_STATEMENT(t, set_operation, SqlSetOperation);
					top_set_opr->operand[1] = t;
					UNPACK_SQL_STATEMENT(top_set_opr, t, set_operation);
					top_set_opr->type = SET_UNION_ALL;
					top_set_opr->operand[0] = old_set_opr_stmt;
				} else {
					// This is a RIGHT_JOIN, so no further action is needed
				}
				UNPACK_SQL_STATEMENT(table_alias_t2, table_alias_stmt2, table_alias);
				UNPACK_SQL_STATEMENT(select_stmt, table_alias_t2->table, select);
				// Make sure all tables have new keys
				UNPACK_SQL_STATEMENT(start_join2, select_stmt->table_list, join);
				cur_join2 = start_join2;
				removed_joins = 0;
				do {
					// If we removed the first table in the list, we need to make sure
					// we update the select statement
					if(start_join2 == NULL) {
						select_stmt->table_list->v.join = cur_join2;
						start_join2 = cur_join2;
					}
					if(cur_join2->next->type == cur_join->type && removed_joins == 0) {
						// Since this is a right JOIN, we actually remove the table *before* this one
						// and turn this table into a normal table
						removed_joins++;
						cur_join2->prev->next = cur_join2->next;
						cur_join2->next->prev = cur_join2->prev;
						cur_join2->next->type = cur_join2->type;
						cur_join2->next->condition = cur_join2->condition;
						if(cur_join2 == start_join2) {
							start_join2 = NULL;
						}
					} else {
						UNPACK_SQL_STATEMENT(table_alias, cur_join2->value, table_alias);
						new_id = (*plan_id)++;
						update_table_references(table_alias_stmt2, table_alias->unique_id, new_id);
						table_alias->unique_id = new_id;
					}
					cur_join2 = cur_join2->next;
				} while(cur_join2 != start_join2);
				replace_table_references(table_alias_stmt2, cur_join->prev->value);

				// Setup the SET operations
				SQL_STATEMENT(select_stmt->set_operation, set_operation_STATEMENT);
				MALLOC_STATEMENT(select_stmt->set_operation, set_operation, SqlSetOperation);
				UNPACK_SQL_STATEMENT(right_set_opr, select_stmt->set_operation, set_operation);
				right_set_opr->type = SET_EXCEPT_ALL;
				right_set_opr->operand[0] = table_alias_stmt2;
				top_set_opr->operand[1] = select_stmt->set_operation;

				table_alias_stmt2 = copy_sql_statement(table_alias_stmt1);
				UNPACK_SQL_STATEMENT(table_alias_t2, table_alias_stmt2, table_alias);
				UNPACK_SQL_STATEMENT(select_stmt, table_alias_t2->table, select);

				// Make sure all tables have new keys
				UNPACK_SQL_STATEMENT(start_join2, select_stmt->table_list, join);
				// Before assigning new keys, make the left table return null values
				//replace_table_references(select_stmt->select_list, cur_join->prev->value);
				replace_table_references(select_stmt->select_list, cur_join->prev->value);
				cur_join2 = start_join2;
				do {
					UNPACK_SQL_STATEMENT(table_alias, cur_join2->value, table_alias);
					new_id = (*plan_id)++;
					update_table_references(table_alias_stmt2, table_alias->unique_id, new_id);
					cur_join2 = cur_join2->next;
				} while(cur_join2 != start_join2);
				right_set_opr->operand[1] = table_alias_stmt2;
				select_stmt->set_operation = NULL;
			}

			insert = lp_generate_set_logical_plan(top_set_opr_stmt, plan_id);
			return insert;
		}
		cur_join = cur_join->next;
	} while(cur_join != start_join);

	MALLOC_LP(insert, LP_INSERT);
	project = MALLOC_LP(insert->v.operand[0], LP_PROJECT);
	select = MALLOC_LP(project->v.operand[1], LP_SELECT);
	criteria = MALLOC_LP(select->v.operand[1], LP_CRITERIA);
	MALLOC_LP(criteria->v.operand[0], LP_KEYS);
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
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);
	cur_join = start_join;
	do {
		if(cur_join->type != INNER_JOIN
		   && cur_join->type != CROSS_JOIN && cur_join->type != NO_JOIN
		   && cur_join->type != NATURAL_JOIN) {
			// Supported OUTER JOINs should have been handled by now; some unsupported
			// types may get here
			FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "OUTER JOIN");
		}
		if(join_right == NULL) {
			join_right = MALLOC_LP(select->v.operand[0], LP_TABLE_JOIN);
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
			join_left = generate_logical_plan(cur_join->value, plan_id);
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
		if(left->v.operand[0]->type == LP_INSERT) {
			UNPACK_SQL_STATEMENT(table_alias, cur_join->value, table_alias);
			lp_replace_derived_table_references(insert, left->v.operand[0], table_alias);
		} else if(left->v.operand[0]->type == LP_SET_OPERATION) {
			UNPACK_SQL_STATEMENT(table_alias, cur_join->value, table_alias);
			t_left = left->v.operand[0]->v.operand[1]->v.operand[0];
			lp_replace_derived_table_references(insert, t_left, table_alias);
			t_right = left->v.operand[0]->v.operand[1]->v.operand[1];
			lp_replace_derived_table_references(insert, t_right, table_alias);
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
