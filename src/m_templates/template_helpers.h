/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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
#define TEMPLATE(name, ...) void name(char **global_buffer, uint64_t *buffer_len, uint64_t *buffer_index, ##__VA_ARGS__)

/// WARNING: this macro assumes the presence of global_buffer, buffer_len, buffer_index
#define TEMPLATE_SNPRINTF(...)                                                                                  \
	do {                                                                                                    \
		boolean_t retry;                                                                                \
		uint64_t  written;                                                                              \
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
#define PP_ORDER_BY	  "\"OrderBy\""
#define PP_GROUP_BY	  "\"GroupBy\""
#define PP_ROW_COUNT	  "\"RowCount\""    /* Note: This has to be maintained in sync with OCTOLIT_ROW_COUNT */
#define PP_KEYS		  "\"keys\""	    /* Note: This has to be maintained in sync with OCTOLIT_KEYS */
#define PP_PARAMETERS	  "\"parameters\""  /* Note: This has to be maintained in sync with OCTOLIT_PARAMETERS */
#define PP_XREF_STATUS	  "\"xref_status\"" /* Note: This has to be maintained in sync with OCTOLIT_XREF_STATUS */
#define PP_OCTO_LEFT_JOIN "octoLeftJoin"    /* Note: This has to be maintained in sync with OCTO_LEFT_JOIN_LIT */
// Set prefixes for YDB global and local variables nodes, i.e. "^" and "", respectively
#define PP_GLOBAL_PREFIX "^"
#define PP_LOCAL_PREFIX	 ""

/* Note: The below PP_* macros do not contain double-quotes within the string literal */
#define PP_COL	      "col"
#define PP_KEY_COLUMN "keyCol"
#define PP_VAL                                                                                \
	"val" /* Note: This variable is easy to read in generated M code but should be used   \
	       * only when we are guaranteed that we won't be invoking user M code. In that   \
	       * case we cannot use such simple variable names as it can cause namespace      \
	       * collisions with user created local variable names. In that case use the      \
	       * PP_YDB_OCTO_* variables as they will have a `%ydbocto` prefix in the name.   \
	       * Makes it harder to read the M code but need it for correctness just in case. \
	       */
#define PP_XREF_COLUMN	       "xrefCol"
#define PP_YDB_OCTO_EXPR       "%ydboctoexpr"
#define PP_YDB_OCTO_G	       "%ydboctog"
#define PP_YDB_OCTO_I	       "%ydboctoi"
#define PP_YDB_OCTO_IN	       "%ydboctoin"
#define PP_YDB_OCTO_P	       "%ydboctop"
#define PP_YDB_OCTO_UPD	       "%ydboctoUPD" /* see comment in tmpl_update_table.ctemplate for its purpose */
#define PP_YDB_OCTO_Z	       "%ydboctoz"
#define PP_YDB_OCTO_ZDUPLICATE "%ydboctozduplicate"
#define PP_YDB_OCTO_ZLIMIT     "%ydboctozlimit"
#define PP_YDB_OCTO_ZDISTINCT  "%ydboctozdistinct"

#define PLAN_LINE_START "    " /* 4 spaces start an M line in the generated plan */

/* Sets output parameters "DELIM" and "IS_DOLLAR_CHAR" based on input parameters "TABLE" "COLUMN" and "IS_TRIGGER" */
#define SET_DELIM_AND_IS_DOLLAR_CHAR(TABLE, COLUMN, IS_TRIGGER, DELIM, IS_DOLLAR_CHAR)      \
	{                                                                                   \
		SqlValue *value;                                                            \
                                                                                            \
		if (COLUMN->delim) {                                                        \
			UNPACK_SQL_STATEMENT(value, COLUMN->delim, value);                  \
			DELIM = value->v.string_literal;                                    \
			IS_DOLLAR_CHAR = (DELIM_IS_DOLLAR_CHAR == DELIM[0] ? TRUE : FALSE); \
			DELIM = &value->v.string_literal[1];                                \
		} else if (TABLE->delim) {                                                  \
			UNPACK_SQL_STATEMENT(keyword, TABLE->delim, keyword);               \
			UNPACK_SQL_STATEMENT(value, keyword->v, value);                     \
			DELIM = value->v.string_literal;                                    \
			IS_DOLLAR_CHAR = (DELIM_IS_DOLLAR_CHAR == DELIM[0] ? TRUE : FALSE); \
			DELIM = &value->v.string_literal[1];                                \
		} else {                                                                    \
			IS_DOLLAR_CHAR = FALSE;                                             \
			DELIM = (IS_TRIGGER ? NULL : COLUMN_DELIMITER);                     \
		}                                                                           \
	}

/* Macro to set a lvn to track which column numbers have had their "PP_COL(i)" fields initialized in generated M code.
 * Sets the "lvName(colNum)" lvn to "" based on input parameters "lvName" and "colNum".
 * Currently used by UPDATE command template M code generation logic.
 */
#define SET_TBL_CONSTRAINT_LVN(lvName, colNum)                                                          \
	{                                                                                               \
		ydb_buffer_t sub;                                                                       \
		char	     numbuf[INT32_TO_STRING_MAX + 1];                                           \
                                                                                                        \
		sub.buf_addr = numbuf;                                                                  \
		sub.len_alloc = sizeof(numbuf);                                                         \
		sub.len_used = snprintf(sub.buf_addr, sub.len_alloc, "%d", colNum);                     \
                                                                                                        \
		int status;                                                                             \
		status = ydb_set_s(lvName, 1, &sub, NULL);                                              \
		assert(YDB_OK == status);                                                               \
		UNUSED(status); /* needed to avoid [-Wunused-but-set-variable] warning from compiler */ \
	}

/* Given a "lvName" and "colNum", this macro sets "isLvnSet" to a non-zero value if "lvName(colNum)" is a defined lvn
 * (by a prior call to SET_TBL_CONSTRAINT_LVN) and 0 otherwise. Currently used by UPDATE command template M code generation logic.
 */
#define IS_TBL_CONSTRAINT_LVN_SET(lvName, colNum, isLvnSet)                                             \
	{                                                                                               \
		ydb_buffer_t sub;                                                                       \
		char	     numbuf[INT32_TO_STRING_MAX + 1];                                           \
                                                                                                        \
		sub.buf_addr = numbuf;                                                                  \
		sub.len_alloc = sizeof(numbuf);                                                         \
		sub.len_used = snprintf(sub.buf_addr, sub.len_alloc, "%d", colNum);                     \
                                                                                                        \
		int status;                                                                             \
		status = ydb_data_s(lvName, 1, &sub, &isLvnSet);                                        \
		assert(YDB_OK == status);                                                               \
		UNUSED(status); /* needed to avoid [-Wunused-but-set-variable] warning from compiler */ \
	}

enum EmitSourceForm {
	EmitSourceForm_Value,
	EmitSourceForm_Trigger,
	EmitSourceForm_Insert,
	EmitSourceForm_NoKeyCol,
	EmitSourceForm_UpdateKeyCol,
	EmitSourceForm_AIM
};

typedef enum {
	InvokeDeferredPlan_ANY_ALL,
	InvokeDeferredPlan_EXISTS,
	InvokeDeferredPlan_IN,
	InvokeDeferredPlan_SELECT_SET_VALUES,
	InvokeDeferredPlan_TABLEJOIN,
} InvokeDeferredPlanType;

typedef enum {
	ConstraintColIList,
	ConstraintColNameList,
	ConstraintColNullList,
	ConstraintColValList,
	ConstraintSetColIList,
	ConstraintColChangedList,
} ConstraintColListType;

typedef enum {
	UniqueConstraintLoopNotUpdate,
	UniqueConstraintLoopUpdate,
	UniqueConstraintLoopMax,
} UniqueConstraintLoopType;

void resize_tmpl_buffer(char **global_buffer, uint64_t *buffer_len, uint64_t *buffer_index);

TEMPLATE(tmpl_print_dots, int dots);
TEMPLATE(tmpl_physical_plan, PhysicalPlan *pplan);
TEMPLATE(tmpl_insert_into, PhysicalPlan *pplan);
TEMPLATE(tmpl_delete_from, PhysicalPlan *pplan);
TEMPLATE(tmpl_delete_record_from_table, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_update_key_source, PhysicalPlan *pplan, boolean_t pre_update);
TEMPLATE(tmpl_update_record_in_table, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_update_table, PhysicalPlan *pplan);
TEMPLATE(tmpl_tablejoin, PhysicalPlan *pplan, LogicalPlan *tablejoin, unsigned int cur_key, boolean_t right_join_second_half,
	 int dot_count, char *tableName, char *columnName);
TEMPLATE(tmpl_rightjoin_key, PhysicalPlan *pplan, unsigned int key_start, unsigned int key_end);
TEMPLATE(tmpl_tablejoin_body, PhysicalPlan *pplan, int dot_count, char *tableName, char *columnName);
TEMPLATE(tmpl_tablejoin_body_group_by, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_tablejoin_on_condition, LogicalPlan *tablejoin, PhysicalPlan *pplan, int *dot_count);
TEMPLATE(tmpl_group_by, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_key_start, SqlKey *key);
TEMPLATE(tmpl_key_finish, SqlKey *key);
TEMPLATE(tmpl_key_end, SqlKey *key);
TEMPLATE(tmpl_key_dollardata_check, PhysicalPlan *pplan, SqlKey *key, boolean_t is_derived_column);
// Outputs: '%ydboctocursor(cursorId,PP_KEYS,key->unique_id,tableName,columnName)'
TEMPLATE(tmpl_key, SqlKey *key);
TEMPLATE(tmpl_key_advance, PhysicalPlan *pplan, SqlKey *key);
TEMPLATE(tmpl_key_source, PhysicalPlan *pplan, SqlKey *key, enum EmitSourceForm form);
TEMPLATE(tmpl_key_source_aim, char *tableName, char *columnName);
TEMPLATE(tmpl_print_expression, LogicalPlan *plan, PhysicalPlan *pplan, int dot_count, int depth);
TEMPLATE(tmpl_print_expression_assignment, LogicalPlan *plan, PhysicalPlan *pplan, int dot_count, int depth);
TEMPLATE(tmpl_column_reference, PhysicalPlan *pplan, SqlColumnAlias *column_alias, boolean_t is_trigger, int dot_count, int depth);
TEMPLATE(tmpl_column_reference_common, PhysicalPlan *pplan, SqlColumnAlias *column_alias, boolean_t is_trigger, int dot_count,
	 int depth, int unique_id, boolean_t *done);
TEMPLATE(tmpl_print_expression_group_by_computation, int group_by_column_num, int dot_count);
TEMPLATE(tmpl_column_list_combine, LogicalPlan *plan, PhysicalPlan *pplan, char *delim, boolean_t str2mval, int dot_count,
	 boolean_t is_asterisk);
TEMPLATE(tmpl_invoke_deferred_plan, InvokeDeferredPlanType invocation_type, LogicalPlan *plan, int dot_count);
TEMPLATE(tmpl_invoke_deferred_plan_setoper, InvokeDeferredPlanType invocation_type, LogicalPlan *plan, int dot_count);
TEMPLATE(tmpl_emit_source, SqlTable *table, char *source, char *parm1, int unique_id, int keys_to_match, enum EmitSourceForm form);
TEMPLATE(tmpl_duplication_check, PhysicalPlan *pplan);
TEMPLATE(tmpl_set_duplication_check, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_order_by_key, int num_cols);
TEMPLATE(tmpl_order_by_sort, PhysicalPlan *pplan, boolean_t is_desc, int num_cols, SqlOptionalKeyword *limit_keyword);
TEMPLATE(tmpl_populate_output_key, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_limit_check, SqlOptionalKeyword *limit_keyword, char *prefix, char *suffix);
TEMPLATE(tmpl_where_or_having_or_on, LogicalPlan *plan, PhysicalPlan *pplan, int dot_count);
TEMPLATE(tmpl_xref_key_columns, int num_key_cols);
TEMPLATE(tmpl_print_group_by_column_reference, PhysicalPlan *pplan, SqlColumnAlias *column_alias, boolean_t in_where_clause,
	 int unique_id, int dot_count, boolean_t *done);
TEMPLATE(tmpl_constraint, PhysicalPlan *pplan, LogicalPlan *lp_constraint, SqlTable *table, int dot_count);
TEMPLATE(tmpl_constraint_col_list, PhysicalPlan *pplan, SqlConstraint *constraint, SqlTable *table, ConstraintColListType list_type,
	 int dot_count, UniqueConstraintLoopType loop_type);
TEMPLATE(tmpl_update_column_reference, PhysicalPlan *pplan, SqlColumn *cur_column, int dot_count);
TEMPLATE(tmpl_identity_column, SqlColumn *column);
#endif
