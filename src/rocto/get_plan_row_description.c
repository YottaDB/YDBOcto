/* Copyright (C) 2018-2019 YottaDB, LLC
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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"


RowDescription *get_plan_row_description(PhysicalPlan *plan) {
	SqlTableAlias *source_table;
	SqlColumnListAlias *cla_cur, *cla_end;
	SqlValue *value;
	RowDescription *ret;
	RowDescriptionParm *parms;
	LogicalPlan *cur_plan, *column_alias;
	int num_columns, i;

	if(plan == NULL)
		return NULL;
	// Count the number of columns in the table
	cur_plan = plan->projection;
	num_columns = 0;
	do {
		assert(cur_plan->type == LP_COLUMN_LIST);
		GET_LP(column_alias, cur_plan, 0, LP_WHERE);
		GET_LP(column_alias, column_alias, 1, LP_COLUMN_LIST_ALIAS);
		num_columns++;
		cur_plan = cur_plan->v.operand[1];
	} while(cur_plan != NULL);

	parms = malloc(sizeof(RowDescriptionParm) * num_columns);
	memset(parms, 0, sizeof(RowDescriptionParm));

	// Setup each parm
	i = 0;
	cur_plan = plan->projection;
	do {
		assert(cur_plan->type == LP_COLUMN_LIST);
		GET_LP(column_alias, cur_plan, 0, LP_WHERE);
		GET_LP(column_alias, column_alias, 1, LP_COLUMN_LIST_ALIAS);
		UNPACK_SQL_STATEMENT(value, column_alias->v.column_list_alias->alias, value);
		// This assumes the SqlValue will outlive this RowDescription
		parms[i].name = value->v.string_literal;
		// We don't currently deal with table_id's, so just set it to zero
		parms[i].table_id = 0;
		parms[i].column_id = 0;
		// Postgres stores information about data types in table;
		//  for now, hard-code them based on a few known ones
		// SELECT oid, typlen FROM pg_type WHERE typname='text';
		parms[i].data_type = 25;
		parms[i].data_type_size = -1;
		// select atttypmod from pg_attribute;
		// All the values seem to be -1, not sure why
		parms[i].type_modifier = -1;
		// 0 = text, 1 = binary
		parms[i].format_code = 0;
		i++;
		cur_plan = cur_plan->v.operand[1];
	} while(cur_plan != NULL);

	ret = make_row_description(parms, num_columns);
	free(parms);
	return ret;
}
