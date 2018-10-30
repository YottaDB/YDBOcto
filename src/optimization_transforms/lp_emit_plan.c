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

int emit_plan_helper(char *buffer, size_t buffer_len, int depth, LogicalPlan *plan);

int lp_emit_plan(char *buffer, size_t buffer_len, LogicalPlan *plan) {
	emit_plan_helper(buffer, buffer_len, 0, plan);
}

int emit_plan_helper(char *buffer, size_t buffer_len, int depth, LogicalPlan *plan) {
	char *buff_ptr = buffer, *table_name = " ", *column_name = " ";
	SqlValue *value;
	SqlJoin *cur_join, *start_join;
	SqlKey *key;
	SqlColumn *column;
	SqlColumnList *cur_column_list, *start_column_list;

	if(plan == NULL)
		return 0;
	SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "%*s%s: ", depth, "", lp_action_type_str[plan->type]);
	switch(plan->type) {
		/*	case LP_TABLE_JOIN:
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "\n");
		break;*/
	case LP_KEY:
		key = plan->v.key;
		if(key->column) {
			UNPACK_SQL_STATEMENT(value, key->column->columnName, value);
			column_name = value->v.string_literal;
		}
		if(key->table) {
			UNPACK_SQL_STATEMENT(value, key->table->tableName, value);
			table_name = value->v.string_literal;
		}
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "\n%*s- table_name: %s\n", depth, "", table_name);
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "%*s- column_name: %s\n", depth, "", column_name);
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "%*s- random_id: %d\n", depth, "", key->random_id);
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "%*s- method: %s\n", depth, "", lp_action_type_str[key->type]);
		break;
	case LP_COLUMN_LIST:
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "\n");
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[0]);
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[1]);
		break;
	case LP_VALUE:
		value = plan->v.value;
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	case LP_TABLE:
		UNPACK_SQL_STATEMENT(value, plan->v.table_alias->alias, value);
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	case LP_COLUMN_ALIAS:
		UNPACK_SQL_STATEMENT(column, plan->v.column_alias->column, column);
		UNPACK_SQL_STATEMENT(value, column->columnName, value);
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	default:
		SAFE_SNPRINTF(buff_ptr, buffer, buffer_len, "\n");
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[0]);
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[1]);
		break;
	}

	return buff_ptr - buffer;
}
