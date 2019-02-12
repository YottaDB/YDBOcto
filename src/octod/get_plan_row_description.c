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

#include "octod.h"
#include "message_formats.h"


RowDescription *get_plan_row_description(PhysicalPlan *plan) {
	SqlTableAlias *source_table;
	SqlColumnListAlias *cla_cur, *cla_end;
	SqlValue *value;
	RowDescriptionParm *parms;
	int num_columns, i;

	if(plan == NULL)
		return NULL;
	// Count the number of columns in the table
	source_table = plan->outputTable;
	UNPACK_SQL_STATEMENT(cla_end, source_table->column_list, column_list_alias);
	cla_cur = cla_end;
	do {
		num_columns++;
		cla_cur = cla_cur->next;
	} while (cla_end != cla_cur);

	parms = malloc(sizeof(RowDescriptionParm) * num_columns);
	memset(parms, 0, sizeof(RowDescriptionParm));

	// Setup each parm
	cla_cur = cla_end;
	i = 0;
	do {
		UNPACK_SQL_STATEMENT(value, cla_cur->alias, value);
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
		cla_cur = cla_cur->next;
		i++;
	} while(cla_end != cla_cur);

	return make_row_description(parms, num_columns);
}
