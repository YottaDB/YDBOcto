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
	return emit_plan_helper(buffer, buffer_len, 0, plan);
}

int emit_plan_helper(char *buffer, size_t buffer_len, int depth, LogicalPlan *plan) {
	char *buff_ptr = buffer, *table_name = " ", *column_name = " ", *data_type_ptr = " ";
	size_t written;
	int table_id;
	SqlValue *value;
	SqlKey *key;

	if(buffer_len == 0)
		return 0;
	if(plan == NULL)
		return 0;
	SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s%s: ", depth, "", lp_action_type_str[plan->type]);
	switch(plan->type) {
	case LP_PIECE_NUMBER:
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%d\n", plan->v.piece_number);
		break;
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
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n%*s- table_name: %s\n", depth, "", table_name);
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- column_name: %s\n", depth, "", column_name);
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- random_id: %d\n", depth, "", key->random_id);
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- method: %s\n", depth, "", lp_action_type_str[key->type]);
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- xref_key: %s\n", depth, "", key->is_cross_reference_key ? "true" :
				"false");
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- uses_xref_key: %s\n", depth, "", key->cross_reference_output_key ? "true" :
				"false");
		if(key->type == LP_KEY_FIX) {
			SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- value:\n", depth, "");
			buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 4, key->value);
		}
		break;
	case LP_COLUMN_LIST:
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[0]);
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[1]);
		break;
	case LP_VALUE:
		value = plan->v.value;
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	case LP_TABLE:
		UNPACK_SQL_STATEMENT(value, plan->v.table_alias->alias, value);
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	case LP_COLUMN_ALIAS:
		if(plan->v.column_alias->column->type == column_STATEMENT) {
			UNPACK_SQL_STATEMENT(value, plan->v.column_alias->column->v.column->columnName, value);
			column_name = value->v.string_literal;
		} else {
			UNPACK_SQL_STATEMENT(value, plan->v.column_alias->column->v.column_list_alias->alias, value);
			column_name = value->v.string_literal;
		}
		UNPACK_SQL_STATEMENT(value, plan->v.column_alias->table_alias->v.table_alias->alias, value);
		table_name = value->v.string_literal;
		table_id = plan->v.column_alias->table_alias->v.table_alias->unique_id;
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s(%d).%s\n", table_name, table_id, column_name);
		break;
	case LP_COLUMN_LIST_ALIAS:
		switch(plan->v.column_list_alias->type) {
			case UNKNOWN_SqlValueType:
				data_type_ptr = "UNKNOWN_SqlValueType";
				break;
			case NUMBER_LITERAL:
				data_type_ptr = "NUMBER_LITERAL";
				break;
			case STRING_LITERAL:
				data_type_ptr = "STRING_LITERAL";
				break;
			case DATE_TIME:
				data_type_ptr = "DATE_TIME";
				break;
			case COLUMN_REFERENCE:
				data_type_ptr = "COLUMN_REFERENCE";
				break;
			case CALCULATED_VALUE:
				data_type_ptr = "CALCULATED_VALUE";
				break;
			case TEMPORARY_TABLE_TYPE:
				data_type_ptr = "TEMPORARY_TABLE_TYPE";
				break;
			case BOOLEAN_VALUE:
				data_type_ptr = "BOOLEAN_VALUE";
				break;
			case FUNCTION_NAME:
				data_type_ptr = "FUNCTION";
				break;
			case PARAMETER_VALUE:
				data_type_ptr = "PARAMETER";
				break;
			case NUL_VALUE:
				data_type_ptr = "NULL";
				break;
		}
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n%*s- type: %s", depth, "", data_type_ptr);
		UNPACK_SQL_STATEMENT(value, plan->v.column_list_alias->alias, value);
		column_name = value->v.string_literal;
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n%*s- alias: %s\n", depth, "", column_name);
		break;
	case LP_KEYWORDS:
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "keywords\n");
		break;
	default:
		SAFE_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[0]);
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.operand[1]);
		break;
	}

	return buff_ptr - buffer;
}
