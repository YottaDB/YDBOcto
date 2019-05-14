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

LogicalPlan *lp_replace_helper(LogicalPlan *where, SqlTableAlias *table_alias, SqlKey *key);

/**
 * Goes through plan and replaces all key unique id's (and references to those keys) with a new, unique key
 */
LogicalPlan *lp_generate_new_keys(LogicalPlan *root, LogicalPlan *new_plan) {
	SqlKey *key;
	LogicalPlan *key_lp, *t;

	key_lp = lp_get_output_key(new_plan);
	key = key_lp->v.key;
	t = lp_get_select_where(root);
	t->v.operand[0] = lp_replace_helper(t->v.operand[0], table_alias, key);
	t = lp_get_project(root);
	t->v.operand[0] = lp_replace_helper(t->v.operand[0], table_alias, key);

	return root;
}

LogicalPlan *lp_replace_helper(LogicalPlan *where, SqlTableAlias *table_alias, int new_unique_id) {
	SqlColumn *column;
	SqlColumnAlias *alias;
	SqlValue *value;
	SqlColumnListAlias *cur_cl_alias, *start_cl_alias, *t_cl_alias;
	SqlTableAlias *t_table_alias;
	LogicalPlan *ret = where;
	char *column_name1, *column_name2;
	int part = 1;

	if(where == NULL)
		return NULL;

	switch(where->type) {
	case LP_COLUMN_ALIAS:
		alias = where->v.column_alias;
		if(alias->table_alias->v.table_alias == table_alias) {
			MALLOC_LP(ret, LP_DERIVED_COLUMN);
			MALLOC_LP(ret->v.operand[0], LP_KEY);
			ret->v.operand[0]->v.key = key;
			part = get_column_piece_number(alias, table_alias);
			MALLOC_LP(ret->v.operand[1], LP_PIECE_NUMBER);
			ret->v.operand[1]->v.piece_number = part;
		}
		break;
	case LP_COLUMN_LIST:
		ret->v.operand[0] = lp_replace_helper(where->v.operand[0], table_alias, key);
		ret->v.operand[1] = lp_replace_helper(where->v.operand[1], table_alias, key);
		break;
	case LP_WHERE:
		ret->v.operand[0] = lp_replace_helper(where->v.operand[0], table_alias, key);
		break;
	case LP_SET_OPERATION:
		// I don't expect this to happen, but seems like a possible path
		assert(FALSE);
		break;
	default:
		if(where->type > LP_ADDITION) {
			ret->v.operand[0] = lp_replace_helper(where->v.operand[0], table_alias, key);
			ret->v.operand[1] = lp_replace_helper(where->v.operand[1], table_alias, key);
		}
		break;
	}
	return ret;
}
