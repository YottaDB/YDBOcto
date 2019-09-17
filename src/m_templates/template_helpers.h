/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

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
	char *buff_ptr = buffer;		\
	int written;

#define TEMPLATE_END()				\
	return buff_ptr - buffer;

enum EmitSourceForm {
	EmitSourceForm_Value,
	EmitSourceForm_Trigger
};

TEMPLATE(print_dots, int dots);
TEMPLATE(tmpl_physical_plan, PhysicalPlan *plan);
TEMPLATE(tmpl_tablejoin, PhysicalPlan *plan, LogicalPlan *tablejoin, unsigned int cur_key, int dot_count,		\
										char *tableName, char *columnName);
TEMPLATE(tmpl_tablejoin_innerjoin, PhysicalPlan *plan, LogicalPlan *tablejoin, int *dot_count);
TEMPLATE(tmpl_tablejoin_leftjoin, PhysicalPlan *plan, LogicalPlan *tablejoin, unsigned int cur_key, int *dot_count);
TEMPLATE(tmpl_tablejoin_body, PhysicalPlan *plan, int dot_count, char *tableName, char *columnName);
TEMPLATE(tmpl_key_start, SqlKey *key);
TEMPLATE(tmpl_key_end, SqlKey *key);
TEMPLATE(tmpl_key, SqlKey *key);
TEMPLATE(tmpl_key_advance, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_key_source, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_temp_key_advance, SqlKey *key);
TEMPLATE(tmpl_print_expression, LogicalPlan *plan, PhysicalPlan *pplan);
TEMPLATE(tmpl_column_reference, PhysicalPlan *pplan, SqlColumnAlias *column_alias);
TEMPLATE(tmpl_column_list_combine, LogicalPlan *plan, PhysicalPlan *pplan, char *delim, char *resume, int resume_length);
TEMPLATE(tmpl_emit_source, char *source, char *table_name, int unique_id, int keys_to_match, enum EmitSourceForm form);
TEMPLATE(tmpl_column_reference_trigger, PhysicalPlan *pplan, SqlColumnAlias *column_alias);
TEMPLATE(tmpl_duplication_check, PhysicalPlan *plan);

#endif
