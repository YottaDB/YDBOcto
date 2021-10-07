/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
/// WARNING: this macro assumes the presence of global_buffer, buffer_len, buffer_index
#define TMPL(fn, ...) fn(global_buffer, buffer_len, buffer_index, ##__VA_ARGS__);

// This does not put a trailing semicolon
#define TEMPLATE(name, ...) void name(char **global_buffer, int *buffer_len, int *buffer_index, ##__VA_ARGS__)

/// WARNING: this macro assumes the presence of global_buffer, buffer_len, buffer_index, written, retry
#define TEMPLATE_SNPRINTF(...)                                                                                  \
	do {                                                                                                    \
		boolean_t retry;                                                                                \
		int	  written;                                                                              \
                                                                                                                \
		retry = FALSE;                                                                                  \
		written = snprintf(*global_buffer + *buffer_index, *buffer_len - *buffer_index, ##__VA_ARGS__); \
		if (written >= *buffer_len - *buffer_index) {                                                   \
			retry = TRUE;                                                                           \
			resize_tmpl_buffer(global_buffer, buffer_len, buffer_index);                            \
			continue;                                                                               \
		}                                                                                               \
		*buffer_index += written;                                                                       \
		if (!retry)                                                                                     \
			break;                                                                                  \
	} while (TRUE);

/* Define PP_* (stands for Physical Plan) macros which correspond to literals (numerics/strings) that are used in
 * various parts of the "*.ctemplate" files in Octo code (all deal with physical plans).
 * Using the macros avoids duplication of the literal.
 */

/* Note: The below PP_* macros contain double-quotes within the string literal (hence the use of \") as they are used
 *       inside the tmpl_*.ctemplate functions. Not having that will cause generated M code to contain just OrderBy
 *       instead of "OrderBy" as the subscript in an lvn.
 */
#define PP_ORDER_BY    "\"OrderBy\""
#define PP_GROUP_BY    "\"GroupBy\""
#define PP_ROW_COUNT   "\"RowCount\""	 /* Note: This has to be maintained in sync with OCTOLIT_ROW_COUNT */
#define PP_KEYS	       "\"keys\""	 /* Note: This has to be maintained in sync with OCTOLIT_KEYS */
#define PP_PARAMETERS  "\"parameters\""	 /* Note: This has to be maintained in sync with OCTOLIT_PARAMETERS */
#define PP_XREF_STATUS "\"xref_status\"" /* Note: This has to be maintained in sync with OCTOLIT_XREF_STATUS */
// Set prefixes for YDB global and local variables nodes, i.e. "^" and "", respectively
#define PP_GLOBAL_PREFIX "^"
#define PP_LOCAL_PREFIX	 ""

/* Note: The below PP_* macros do not contain double-quotes within the string literal */
#define PP_COL		       "col"
#define PP_KEY_COLUMN	       "keyCol"
#define PP_VAL		       "val"
#define PP_XREF_COLUMN	       "xrefCol"
#define PP_YDB_OCTO_EXPR       "%ydboctoexpr"
#define PP_YDB_OCTO_G	       "%ydboctog"
#define PP_YDB_OCTO_I	       "%ydboctoi"
#define PP_YDB_OCTO_IN	       "%ydboctoin"
#define PP_YDB_OCTO_P	       "%ydboctop"
#define PP_YDB_OCTO_Z	       "%ydboctoz"
#define PP_YDB_OCTO_ZDUPLICATE "%ydboctozduplicate"
#define PP_YDB_OCTO_ZLIMIT     "%ydboctozlimit"
#define PP_YDB_OCTO_ZDISTINCT  "%ydboctozdistinct"

#define PLAN_LINE_START "    " /* 4 spaces start an M line in the generated plan */

/* This macro is currently unused but preserved in the hope that it might be needed in the near future */
#define IS_COLUMN_NOT_NULL(COLUMN)                                                                     \
	((NULL != get_keyword(COLUMN, PRIMARY_KEY)) || (NULL != get_keyword(COLUMN, OPTIONAL_KEY_NUM)) \
	 || (NULL != get_keyword(COLUMN, NOT_NULL)))

enum EmitSourceForm {
	EmitSourceForm_Value,
	EmitSourceForm_Trigger,
	EmitSourceForm_Insert,
	EmitSourceForm_NoKeyCol,
};

typedef enum {
	InvokeDeferredPlan_ANY_ALL,
	InvokeDeferredPlan_EXISTS,
	InvokeDeferredPlan_IN,
	InvokeDeferredPlan_SELECT_SET_VALUES,
	InvokeDeferredPlan_TABLEJOIN,
} InvokeDeferredPlanType;

void resize_tmpl_buffer(char **global_buffer, int *buffer_len, int *buffer_index);

TEMPLATE(tmpl_print_dots, int dots);
TEMPLATE(tmpl_physical_plan, PhysicalPlan *plan);
TEMPLATE(tmpl_insert_into, PhysicalPlan *plan);
TEMPLATE(tmpl_delete_from, PhysicalPlan *plan);
TEMPLATE(tmpl_delete_record_from_table, PhysicalPlan *plan, int dot_count);
TEMPLATE(tmpl_tablejoin, PhysicalPlan *plan, LogicalPlan *tablejoin, unsigned int cur_key, boolean_t right_join_second_half,
	 int dot_count, char *tableName, char *columnName);
TEMPLATE(tmpl_rightjoin_key, PhysicalPlan *plan, unsigned int key_start, unsigned int key_end);
TEMPLATE(tmpl_tablejoin_body, PhysicalPlan *plan, int dot_count, char *tableName, char *columnName);
TEMPLATE(tmpl_tablejoin_body_group_by, PhysicalPlan *plan, int dot_count);
TEMPLATE(tmpl_tablejoin_on_condition, LogicalPlan *tablejoin, PhysicalPlan *plan, int *dot_count);
TEMPLATE(tmpl_group_by, PhysicalPlan *plan, int dot_count);
TEMPLATE(tmpl_key_start, SqlKey *key);
TEMPLATE(tmpl_key_end, SqlKey *key);
// Outputs: '%ydboctocursor(cursorId,PP_KEYS,key->unique_id,tableName,columnName)'
TEMPLATE(tmpl_key, SqlKey *key);
TEMPLATE(tmpl_key_advance, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_key_source, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_print_expression, LogicalPlan *plan, PhysicalPlan *pplan, int dot_count, int depth);
TEMPLATE(tmpl_print_expression_assignment, LogicalPlan *plan, PhysicalPlan *pplan, int dot_count, int depth);
TEMPLATE(tmpl_column_reference, PhysicalPlan *pplan, SqlColumnAlias *column_alias, boolean_t is_trigger, int dot_count);
TEMPLATE(tmpl_column_list_combine, LogicalPlan *plan, PhysicalPlan *pplan, char *delim, boolean_t str2mval, int dot_count,
	 boolean_t is_asterisk);
TEMPLATE(tmpl_invoke_deferred_plan, InvokeDeferredPlanType invocation_type, LogicalPlan *plan, int dot_count);
TEMPLATE(tmpl_invoke_deferred_plan_setoper, InvokeDeferredPlanType invocation_type, LogicalPlan *plan, int dot_count);
TEMPLATE(tmpl_emit_source, SqlTable *table, char *source, char *table_name, int unique_id, int keys_to_match,
	 enum EmitSourceForm form);
TEMPLATE(tmpl_duplication_check, PhysicalPlan *plan);
TEMPLATE(tmpl_order_by_key, int num_cols);
TEMPLATE(tmpl_populate_output_key, PhysicalPlan *plan, int dot_count);
TEMPLATE(tmpl_limit_check, SqlOptionalKeyword *limit_keyword, char *prefix, char *suffix);
TEMPLATE(tmpl_where_or_having_or_on, LogicalPlan *plan, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_xref_key_columns, int num_key_cols);

#endif
