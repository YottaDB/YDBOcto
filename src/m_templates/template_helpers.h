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
#ifndef TEMPLATE_HELPERS_H
#define TEMPLATE_HELPERS_H

#include "logical_plan.h"
#include "physical_plan.h"

// Macros to make life easier
/// WARNING: this macro assumes the presence of buffer, buff_ptr, and buffer_len
#define TMPL(fn, ...)						\
	buff_ptr += fn(buff_ptr,				\
		       buffer_len - (buff_ptr - buffer),	\
		       ## __VA_ARGS__);

// This does not put a trailing semicolon
#define TEMPLATE(name, ...)					\
	int name(char *buffer, int buffer_len, ## __VA_ARGS__)

#define TEMPLATE_INIT()				\
	char *buff_ptr = buffer;

#define TEMPLATE_END()				\
	return buff_ptr - buffer;

TEMPLATE(print_dots, int dots);
TEMPLATE(tmpl_physical_plan, PhysicalPlan *plan);
TEMPLATE(tmpl_key_start, SqlKey *key);
TEMPLATE(tmpl_key_end, SqlKey *key);
TEMPLATE(tmpl_key, SqlKey *key);
TEMPLATE(tmpl_key_advance, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_temp_key_advance, SqlKey *key);
TEMPLATE(tmpl_print_expression, LogicalPlan *plan, PhysicalPlan *pplan);
TEMPLATE(tmpl_column_reference, PhysicalPlan *pplan, SqlColumnAlias *column_alias);
TEMPLATE(tmpl_column_list_combine, LogicalPlan *plan, PhysicalPlan *pplan, char *delim);
TEMPLATE(tmpl_emit_source, char *source, char *table_name, int unique_id, int keys_to_match);

#endif
