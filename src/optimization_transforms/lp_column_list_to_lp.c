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

LogicalPlan *lp_column_list_to_lp(SqlColumnListAlias *list, int *plan_id) {
	LogicalPlan *column_list, *ret_column_list = NULL;
	LogicalPlan *where;
	LogicalPlan *column_list_alias;
	SqlColumnListAlias *cur_column_list, *start_column_list, *next_column_list_alias;
	SqlColumnList *t_column_list;
	assert(list != NULL);

	MALLOC_LP(column_list, LP_COLUMN_LIST);

	cur_column_list = start_column_list = list;
	do {
		where = MALLOC_LP(column_list->v.operand[0], LP_WHERE);
		/// TODO: handle the absence of prev
		UNPACK_SQL_STATEMENT(t_column_list, cur_column_list->column_list, column_list);
		where->v.operand[0] = lp_generate_where(t_column_list->value, plan_id);
		column_list_alias = MALLOC_LP(where->v.operand[1], LP_COLUMN_LIST_ALIAS);
		// When we do this copy, we only want a single CLA; this prevents the copy from
		//   grabbing more
		next_column_list_alias = cur_column_list->next;
		column_list_alias->v.column_list_alias = (SqlColumnListAlias*)malloc(sizeof(SqlColumnListAlias));
		memset(column_list_alias->v.column_list_alias, 0, sizeof(SqlColumnListAlias));
		column_list_alias->v.column_list_alias->alias = copy_sql_statement(cur_column_list->alias);
		column_list_alias->v.column_list_alias->type = cur_column_list->type;
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
