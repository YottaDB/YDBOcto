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

LogicalPlan *generate_lp_where(SqlStatement *stmt, int *plan_id);
/**
 * Generates a LP for the given column list, with a root element of
 *  LP_COLUMN_LIST and child elements of LP_WHERE and LP_COLUMN_LIST
 */
LogicalPlan *column_list_to_lp(SqlColumnListAlias *list);
SqlColumnListAlias *columns_to_column_list(SqlColumn *column, SqlTableAlias *table_alias);
LogicalPlan *table_join_to_column_list(LogicalPlan *table_join);

LogicalPlan *generate_logical_plan(SqlStatement *stmt, int *plan_id) {
	SqlSelectStatement *select_stmt;
	LogicalPlan *insert, *project, *column_list, *select, *dst, *dst_key;
	LogicalPlan *criteria, *table, *keys, *where, *order_by, *select_options;
	LogicalPlan *join_left, *join_right, *temp, *select_right, *select_left;
	LogicalPlan *start_join_condition = NULL, *cur_join_condition = NULL, *t_join_condition;
	LogicalPlan *keywords;
	SqlJoin *cur_join, *start_join;
	SqlColumnListAlias *list;
	SqlTableAlias *table_alias;

	UNPACK_SQL_STATEMENT(select_stmt, stmt, select);
	UNPACK_SQL_STATEMENT(start_join, select_stmt->table_list, join);
	//MALLOC_LP(start_join_condition, LP_WHERE);
	//start_join_condition->v.operand[0] = generate_lp_where(start_join->condition, plan_id);

	MALLOC_LP(insert, LP_INSERT);
	insert->counter = plan_id;
	project = MALLOC_LP(insert->v.operand[0], LP_PROJECT);
	dst = MALLOC_LP(insert->v.operand[1], LP_OUTPUT);
	dst_key = MALLOC_LP(dst->v.operand[0], LP_KEY);
	dst_key->v.key = (SqlKey*)malloc(sizeof(SqlKey));
	memset(dst_key->v.key, 0, sizeof(SqlKey));
	dst_key->v.key->random_id = get_plan_unique_number(insert);
	if(select_stmt->order_expression != NULL) {
		order_by = MALLOC_LP(dst->v.operand[1], LP_COLUMN_LIST);
		UNPACK_SQL_STATEMENT(list, select_stmt->order_expression, column_list_alias);
		order_by->v.operand[0] = column_list_to_lp(list);
	}
	/// TODO: we should look at the columns to decide which values
	//   are keys, and if none, create a rowId as part of the advance
	dst_key->v.key->type = LP_KEY_ADVANCE;
	select = MALLOC_LP(project->v.operand[1], LP_SELECT);
	//join_right = table = MALLOC_LP(select->v.operand[0], LP_TABLE_JOIN);
	join_right = NULL;
	cur_join = start_join;
	do {
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
			t_join_condition->v.operand[0] = generate_lp_where(cur_join->condition, plan_id);
			start_join_condition = lp_join_where(start_join_condition, t_join_condition);
		}
		cur_join = cur_join->next;
	} while(cur_join != start_join);

	if(select_stmt->select_list->v.column_list == NULL) {
		temp = table_join_to_column_list(select->v.operand[0]);
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
		temp = column_list_to_lp(list);
	}
	project->v.operand[0] = temp;

	criteria = MALLOC_LP(select->v.operand[1], LP_CRITERIA);
	keys = MALLOC_LP(criteria->v.operand[0], LP_KEYS);
	select_options = MALLOC_LP(criteria->v.operand[1], LP_SELECT_OPTIONS);
	where = MALLOC_LP(select_options->v.operand[0], LP_WHERE);

	// The actual work of populating the plan
	where->v.operand[0] = generate_lp_where(select_stmt->where_expression, plan_id);
	if(start_join_condition) {
		where = lp_join_where(where, start_join_condition);
		select_options->v.operand[0] = where;
	}
	where->v.operand[1] = NULL;

	keywords = MALLOC_LP(select_options->v.operand[1], LP_KEYWORDS);
	UNPACK_SQL_STATEMENT(keywords->v.keywords, select_stmt->optional_words, keyword);

	// At this point, we need to populate keys
	//  Before we can do that, we need to resolve the JOINs, which are
	//  an area where we definetely want to look at optimization
	//  therefore, we leave the plan in a semi-valid state
	return insert;
}

LogicalPlan *generate_lp_where(SqlStatement *stmt, int *plan_id) {
	LogicalPlan *ret = NULL;
	LPActionType type;
	SqlValue *value;
	SqlBinaryOperation *binary;

	if(stmt == NULL)
		return NULL;

	switch(stmt->type) {
	case select_STATEMENT:
		ret = generate_logical_plan(stmt, plan_id);
		/// TODO: should this be moved to the optimize phase for this plan?
		ret = optimize_logical_plan(ret);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			ret = generate_lp_where(value->v.calculated, plan_id);
			break;
		default:
			MALLOC_LP(ret, LP_VALUE);
			ret->v.value = value;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		/// WARNING: we simply add the enum offset to find the type
		type = binary->operation + LP_ADDITION;
		MALLOC_LP(ret, type);
		ret->v.operand[0] = generate_lp_where(binary->operands[0], plan_id);
		ret->v.operand[1] = generate_lp_where(binary->operands[1], plan_id);
		break;
	case column_alias_STATEMENT:
		MALLOC_LP(ret, LP_COLUMN_ALIAS);
		ret->v.column_alias = stmt->v.column_alias;
		/// TODO: free stmt?
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
	}

	return ret;
}

/**
 * Walk if we have a list, append it to insert.project; else, walk through the
 *  list of tables and either copy subquery columns or generate new ones
 *
 * @returns a LogicalPlan of type LP_COLUMN_LIST which contains a series of
 *  LP_WHEREs
 */
LogicalPlan *table_join_to_column_list(LogicalPlan *table_join) {
	LogicalPlan *ret;
	LogicalPlan *t;
	LogicalPlan *next_insert, *next_project, *next_column_list;
	SqlTable *table;
	SqlColumn *columns;
	SqlColumnListAlias *sql_column_list;
	SqlTableAlias *table_alias;
	SqlSelectStatement *select;

	if(table_join == NULL)
		return NULL;
	assert(table_join->type == LP_TABLE_JOIN);
	assert(table_join->v.operand[0] != NULL);
	t = table_join->v.operand[0];

	switch(t->type) {
	case LP_INSERT:
		GET_LP(next_project, t, 0, LP_PROJECT);
		GET_LP(next_column_list, next_project, 0, LP_COLUMN_LIST);
		ret = lp_copy_plan(next_column_list);
		break;
	case LP_TABLE:
		/// TODO: handle column aliases
		table_alias = t->v.table_alias;
		if(table_alias->table->type == table_STATEMENT) {
			UNPACK_SQL_STATEMENT(table, table_alias->table, table);
			UNPACK_SQL_STATEMENT(columns, table->columns, column);
			sql_column_list = columns_to_column_list(columns, table_alias);
		} else if(table_alias->table->type == select_STATEMENT) {
			// This case is handled by a recursive call to generate_logical_plan
			//  and will need to be insert a bit further up
			return NULL;
		} else {
			assert(FALSE);
		}
		ret = column_list_to_lp(sql_column_list);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}

	if(table_join->v.operand[1]) {
		t = ret;
		while(t->v.operand[1]) {
			t = t->v.operand[1];
		}
		t->v.operand[1] = table_join_to_column_list(table_join->v.operand[1]);
	}
	return ret;
}

SqlColumnListAlias *columns_to_column_list(SqlColumn *column, SqlTableAlias *table_alias) {
	SqlColumnList *cur, *t_column_list;
	SqlColumnListAlias *ret = NULL, *t_column_list_alias, *cur_column_list_alias;
	SqlStatement *stmt;
	SqlColumnAlias *alias;
	SqlColumn *cur_column, *start_column;

	cur_column = start_column = column;
	do {
		cur = (SqlColumnList*)malloc(sizeof(SqlColumnList));
		memset(cur, 0, sizeof(SqlColumnList));
		dqinit(cur);
		SQL_STATEMENT(stmt, column_alias_STATEMENT);
		MALLOC_STATEMENT(stmt, column_alias, SqlColumnAlias);
		cur->value = stmt;

		alias = stmt->v.column_alias;
		PACK_SQL_STATEMENT(alias->column, cur_column, column);

		cur_column_list_alias = (SqlColumnListAlias*)malloc(sizeof(SqlColumnListAlias));
		memset(cur_column_list_alias, 0, sizeof(SqlColumnListAlias));
		cur_column_list_alias->alias = copy_sql_statement(cur_column->columnName);
		PACK_SQL_STATEMENT(cur_column_list_alias->column_list, cur, column_list);
		dqinit(cur_column_list_alias);
		PACK_SQL_STATEMENT(alias->table_alias, table_alias, table_alias);
		if(ret == NULL) {
			ret = cur_column_list_alias;
		} else {
			dqinsert(ret, cur_column_list_alias, t_column_list_alias);
		}
		cur_column = cur_column->next;
	} while(cur_column != start_column);
	assert(ret != NULL);
	return ret;
}

LogicalPlan *column_list_to_lp(SqlColumnListAlias *list) {
	LogicalPlan *column_list, *ret_column_list = NULL;
	LogicalPlan *where;
	SqlColumnListAlias *cur_column_list, *start_column_list;
	SqlColumnList *t_column_list;
	assert(list != NULL);

	MALLOC_LP(column_list, LP_COLUMN_LIST);

	cur_column_list = start_column_list = list;
	do {
		where = MALLOC_LP(column_list->v.operand[0], LP_WHERE);
		/// TODO: handle the absence of prev
		UNPACK_SQL_STATEMENT(t_column_list, cur_column_list->column_list, column_list);
		where->v.operand[0] = generate_lp_where(t_column_list->value, NULL);
		cur_column_list = cur_column_list->next;
		if(ret_column_list == NULL)
			ret_column_list = column_list;
		if(cur_column_list != start_column_list) {
			MALLOC_LP(column_list->v.operand[1], LP_COLUMN_LIST);
			column_list = column_list->v.operand[1];
		}
	} while(cur_column_list != start_column_list);


	return ret_column_list;
}
