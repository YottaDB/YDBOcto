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
/// WARNING: this macro assumes the presence of gloabl_buffer, buffer_len, buffer_index
#define TMPL(fn, ...)						\
	fn(global_buffer, buffer_len, buffer_index,		\
		       ## __VA_ARGS__);

// This does not put a trailing semicolon
#define TEMPLATE(name, ...)					\
	void name(char **global_buffer, int *buffer_len, int *buffer_index, ## __VA_ARGS__)

#define TEMPLATE_INIT()				\
	int written, retry = FALSE;

#define TEMPLATE_END()				\
	return;

/// WARNING: this macro assumes the presence of gloabl_buffer, buffer_len, buffer_index, written, retry
#define TEMPLATE_SNPRINTF(...) 													\
		do { 														\
			retry = FALSE; 												\
			written = snprintf(*global_buffer + *buffer_index, *buffer_len - *buffer_index, ## __VA_ARGS__);	\
			if (written >= *buffer_len - *buffer_index) { 								\
				retry = TRUE; 											\
				resize_tmpl_buffer(global_buffer, buffer_len, buffer_index); 					\
				continue; 											\
			}													\
			*buffer_index += written;										\
		} while(retry);


enum EmitSourceForm {
	EmitSourceForm_Value,
	EmitSourceForm_Trigger
};

void resize_tmpl_buffer(char **global_buffer, int *buffer_len, int *buffer_index);

TEMPLATE(tmpl_print_dots, int dots);
TEMPLATE(tmpl_physical_plan, PhysicalPlan *plan);
TEMPLATE(tmpl_tablejoin, PhysicalPlan *plan, LogicalPlan *tablejoin, unsigned int cur_key, boolean_t right_join_second_half,	\
								int dot_count, char *tableName, char *columnName);
TEMPLATE(tmpl_tablejoin_innerjoin, PhysicalPlan *plan, LogicalPlan *tablejoin, int *dot_count);
TEMPLATE(tmpl_tablejoin_leftjoin, PhysicalPlan *plan, LogicalPlan *tablejoin, unsigned int cur_key, int *dot_count);
TEMPLATE(tmpl_tablejoin_rightjoin, PhysicalPlan *plan, LogicalPlan *tablejoin, boolean_t skip_on_condition,	\
			unsigned int key_start, unsigned int key_end, int *dot_count);
TEMPLATE(tmpl_rightjoin_key, PhysicalPlan *plan, unsigned int key_start, unsigned int key_end);
TEMPLATE(tmpl_tablejoin_body, PhysicalPlan *plan, int dot_count, char *tableName, char *columnName);
TEMPLATE(tmpl_key_start, SqlKey *key);
TEMPLATE(tmpl_key_end, SqlKey *key);
TEMPLATE(tmpl_key, SqlKey *key);
TEMPLATE(tmpl_key_advance, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_key_source, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_temp_key_advance, SqlKey *key);
TEMPLATE(tmpl_print_expression, LogicalPlan *plan, PhysicalPlan *pplan);
TEMPLATE(tmpl_column_reference, PhysicalPlan *pplan, SqlColumnAlias *column_alias);
TEMPLATE(tmpl_column_list_combine, LogicalPlan *plan, PhysicalPlan *pplan, char *delim, int resume, int resume_length);
TEMPLATE(tmpl_emit_source, char *source, char *table_name, int unique_id, int keys_to_match, enum EmitSourceForm form);
TEMPLATE(tmpl_column_reference_trigger, PhysicalPlan *pplan, SqlColumnAlias *column_alias);
TEMPLATE(tmpl_duplication_check, PhysicalPlan *plan);
TEMPLATE(tmpl_order_by_key, int num_cols);

#endif
