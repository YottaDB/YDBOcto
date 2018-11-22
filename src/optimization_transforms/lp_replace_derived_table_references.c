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
 * Goes through plan replaces LP_COLUMN references to the given table alias
 *  with a LP_DERIVED_COLUMN and a LP_PIECE_NUMBER/LP_KEY combination
 */
LogicalPlan *lp_replace_derived_table_references(LogicalPlan *root, LogicalPlan *new_plan, SqlTableAlias *table_alias) {
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

LogicalPlan *lp_replace_helper(LogicalPlan *where, SqlTableAlias *table_alias, SqlKey *key) {
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
			/*
			// Search for the part number; unless the column is a key, then just use that
			if(alias->column->type == column_STATEMENT) {
				UNPACK_SQL_STATEMENT(column, alias->column, column);
				UNPACK_SQL_STATEMENT(value, column->columnName, value);
				column_name1 = value->v.string_literal;
			}
			UNPACK_SQL_STATEMENT(start_cl_alias, table_alias->column_list, column_list_alias);
			cur_cl_alias = start_cl_alias;
			do {
				// Verify that this alias of column_list_alias
				if(alias->column->type == column_STATEMENT) {
					UNPACK_SQL_STATEMENT(value, cur_cl_alias->alias, value);
					column_name2 = value->v.string_literal;
					if(strcmp(column_name1, column_name2) == 0) {
						break;
					}
				} else {
					UNPACK_SQL_STATEMENT(t_cl_alias, alias->column, column_list_alias);
					if(cur_cl_alias == t_cl_alias) {
						break;
					}
				}
				part++;
				cur_cl_alias = cur_cl_alias->next;
			} while(cur_cl_alias != start_cl_alias);
			assert(part == 1 || cur_cl_alias != start_cl_alias);*/
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
	default:
		if(where->type > LP_ADDITION) {
			ret->v.operand[0] = lp_replace_helper(where->v.operand[0], table_alias, key);
			ret->v.operand[1] = lp_replace_helper(where->v.operand[1], table_alias, key);
		}
		break;
	}
	return ret;
}
