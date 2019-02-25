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
 * Walk if we have a list, append it to insert.project; else, walk through the
 *  list of tables and either copy subquery columns or generate new ones
 *
 * @returns a LogicalPlan of type LP_COLUMN_LIST which contains a series of
 *  LP_WHEREs
 */
LogicalPlan *lp_table_join_to_column_list(LogicalPlan *table_join, int *plan_id) {
	LogicalPlan *ret;
	LogicalPlan *t;
	LogicalPlan *next_insert, *next_project, *next_column_list;
	LogicalPlan *cur, *next;
	SqlTable *table;
	SqlColumn *columns;
	SqlColumnListAlias *sql_column_list;
	SqlTableAlias *table_alias;
	SqlSelectStatement *select;
	SqlKey *key;
	int part = 1;

	if(table_join == NULL)
		return NULL;
	assert(table_join->type == LP_TABLE_JOIN);
	assert(table_join->v.operand[0] != NULL);
	t = table_join->v.operand[0];

	switch(t->type) {
	case LP_INSERT:
		GET_LP(next_project, t, 0, LP_PROJECT);
		GET_LP(next_column_list, next_project, 0, LP_COLUMN_LIST);
		key = lp_get_output_key(t)->v.key;
		//ret = lp_copy_plan(next_column_list);
		// Copy these in as derived columns
		cur = next_column_list;
		next = MALLOC_LP(ret, LP_COLUMN_LIST);
		while(cur != NULL) {
			t = MALLOC_LP(next->v.operand[0], LP_DERIVED_COLUMN);
			MALLOC_LP(t->v.operand[0], LP_KEY);
			t->v.operand[0]->v.key = key;
			MALLOC_LP(t->v.operand[1], LP_PIECE_NUMBER);
			t->v.operand[1]->v.piece_number = part;
			part++;
			cur = cur->v.operand[1];
		}
		break;
	case LP_TABLE:
		/// TODO: handle column aliases
		table_alias = t->v.table_alias;
		if(table_alias->table->type == table_STATEMENT) {
			UNPACK_SQL_STATEMENT(table, table_alias->table, table);
			UNPACK_SQL_STATEMENT(columns, table->columns, column);
			sql_column_list = lp_columns_to_column_list(columns, table_alias);
		} else if(table_alias->table->type == select_STATEMENT) {
			// This case is handled by a recursive call to generate_logical_plan
			//  and will need to be insert a bit further up
			return NULL;
		} else {
			assert(FALSE);
		}
		ret = lp_column_list_to_lp(sql_column_list, plan_id);
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
		t->v.operand[1] = lp_table_join_to_column_list(table_join->v.operand[1], plan_id);
	}
	return ret;
}
