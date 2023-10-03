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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>

#include "octo.h"
#include "template_strings.h"
#include "logical_plan.h"
#include "physical_plan.h"
#include "lp_verify_structure.h"
#include "helpers.h"

PSQL_TypeSize get_type_size_from_psql_type(PSQL_TypeOid type) {
	switch (type) {
	case PSQL_TypeOid_int4:
		return PSQL_TypeSize_int4;
		break;
	case PSQL_TypeOid_numeric:
		return PSQL_TypeSize_numeric;
		break;
	case PSQL_TypeOid_varchar:
		return PSQL_TypeSize_varchar;
		break;
	case PSQL_TypeOid_unknown:
		return PSQL_TypeSize_unknown;
		break;
	case PSQL_TypeOid_bool:
		return PSQL_TypeSize_bool;
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return PSQL_TypeSize_unknown;
		break;
	}
}

/**
 * Emits M code for executing various SQL statements. Previously named "emit_select_statement".
 *
 * @param cursor_exe_global an array of size 3
 * Returns a table describing the temporary table containing the resulting
 *  values
 */
PhysicalPlan *emit_sql_statement(SqlStatement *stmt, char *plan_filename) {
	LogicalPlan *	    plan, *cur_plan, *column_alias, *function, *table, *view;
	PhysicalPlan *	    pplan;
	SqlValue *	    value;
	char		    valbuff[INT32_TO_STRING_MAX];
	int		    status;
	int16_t		    num_columns = 0;
	ydb_buffer_t	    plan_meta[6], value_buffer;
	PhysicalPlanOptions physical_options;
	PSQL_TypeOid	    column_type;
	PSQL_TypeSize	    type_size;

	TRACE(INFO_ENTERING_FUNCTION, "emit_sql_statement");
	assert(stmt
	       && ((table_alias_STATEMENT == stmt->type) || (set_operation_STATEMENT == stmt->type)
		   || (insert_STATEMENT == stmt->type) || (delete_from_STATEMENT == stmt->type)
		   || (update_STATEMENT == stmt->type)));
	plan = generate_logical_plan(stmt);
	if (NULL == plan) {
		return NULL;
	}
	// Kill %ydboctoViewLp created by generate_logical_plan() invocation
	ydb_buffer_t ydboctoViewLp;
	YDB_LITERAL_TO_BUFFER("%ydboctoViewLp", &ydboctoViewLp);

	status = ydb_delete_s(&ydboctoViewLp, 0, NULL, YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		assert(FALSE);
		return NULL;
	}
	lp_emit_plan(plan, "BEFORE optimize_logical_plan()");
	if (!lp_verify_structure(plan, NULL)) {
		ERROR(ERR_PLAN_NOT_WELL_FORMED, "");
		return NULL;
	}
	OPTIMIZE_LOGICAL_PLAN_OUTERMOST_CALL(plan, FALSE);
	if (NULL == plan) {
		ERROR(ERR_FAILED_TO_OPTIMIZE_PLAN, "");
		return NULL;
	}
	lp_emit_plan(plan, "AFTER optimize_logical_plan()");
	memset(&physical_options, 0, sizeof(PhysicalPlanOptions));
	pplan = NULL;
	physical_options.last_plan = &pplan;
	function = NULL;
	table = NULL;
	view = NULL;
	physical_options.function = &function; /* Store pointer to "function" in physical_options.function. "generate_physical_plan"
						* call below will update "function" to point to the start of the linked list of
						* LP_FUNCTION_CALL usages (if any) in the the entire query.
						*/
	physical_options.table = &table; /* Store pointer to "table" in physical_options.table. "generate_physical_plan()" call
					  * below will update "table" to point to the start of the linked list of LP_TABLE usages
					  * (if any) in the the entire query (actual update happens in "lp_verify_structure()"
					  * which is called inside "generate_physical_plan()").
					  */
	physical_options.view = &view;	 /* Store pointer to "view" in physical_options.view. "generate_physical_plan()" call below
					  * will update "view" to point to the start of the linked list of LP_VIEW usages (if any)
					  * in the entire query (actual update happens in "lp_verify_structure()" which is called
					  * inside "generate_physical_plan()").
					  */
	pplan = generate_physical_plan(plan, &physical_options);
	if (NULL == pplan) {
		ERROR(ERR_PLAN_NOT_GENERATED, "physical");
		return NULL;
	}
	assert(NULL != plan_filename);
	status = emit_physical_plan(pplan, plan_filename);
	if (YDB_OK != status)
		return NULL;
	// Prepare metadata buffers
	YDB_STRING_TO_BUFFER(config->global_names.octo, &plan_meta[0]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &plan_meta[1]);
	YDB_STRING_TO_BUFFER(plan_filename, &plan_meta[2]);
	/* Kill any prior data for this given plan (in the rare case it exists).
	 * This will fix any out-of-sync situation between the plan and corresponding db nodes.
	 */
	status = ydb_delete_s(&plan_meta[0], 2, &plan_meta[1], YDB_DEL_TREE);
	/* Ignore any non-zero return value from "ydb_delete_s". Just proceed with "ydb_set_s". */
	UNUSED(status); /* UNUSED macro needed to avoid unused-variable warning from clang-analyzer */
	if (NULL != function) {
		/* "function" points to the start of the linked list of LP_FUNCTION_CALL usages in entire query.
		 * Store link between plan and all function calls that plan uses so a later CREATE/DROP FUNCTION
		 * of any of the functions in this plan knows to delete this stale plan.
		 */
		ydb_buffer_t func_buff[5];

		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &func_buff[0]);
		func_buff[3] = plan_meta[1];
		func_buff[4] = plan_meta[2];
		do {
			LogicalPlan *function_name;
			LogicalPlan *function_hash;

			/* Store function name in "func_buff[1]" */
			GET_LP(function_name, function, 0, LP_VALUE);
			value = function_name->v.lp_value.value;
			assert(STRING_LITERAL == value->type);
			YDB_STRING_TO_BUFFER(value->v.string_literal, &func_buff[1]);
			GET_LP(function_hash, function->v.lp_default.operand[1], 0, LP_VALUE);
			value = function_hash->v.lp_value.value;
			assert(FUNCTION_HASH == value->type);
			YDB_STRING_TO_BUFFER(value->v.string_literal, &func_buff[2]);
			/* Store gvn that links plan and this function */
			status = ydb_set_s(&plan_meta[0], 5, func_buff, NULL);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
			function = function->extra_detail.lp_function_call.next_function;
		} while (LP_LIST_END != function);
		if (YDB_OK != status) {
			return NULL;
		}
	}
	if (NULL != table) {
		/* "table" points to the start of the linked list of LP_TABLE usages in entire query.
		 * Store link between plan and all tables that plan uses so a later CREATE/DROP TABLE
		 * of any of the tables in this plan knows to delete this stale plan.
		 */
		ydb_buffer_t table_buff[3];

		YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLEPLANS, &table_buff[0]);
		table_buff[2] = plan_meta[2];
		do {
			SqlStatement *table_stmt, *tableName;
			SqlTable *    sql_table;

			/* Store table name in "table_buff[1]" */
			assert(LP_TABLE == table->type);
			table_stmt = table->v.lp_table.table_alias->table;
			UNPACK_SQL_STATEMENT(sql_table, table_stmt, create_table);
			tableName = sql_table->tableName;
			UNPACK_SQL_STATEMENT(value, tableName, value);
			assert(COLUMN_REFERENCE == value->type); /* That is the type the lexer uses to store table names */
			YDB_STRING_TO_BUFFER(value->v.string_literal, &table_buff[1]);
			/* Store gvn that links plan and this table */
			status = ydb_set_s(&plan_meta[0], 3, table_buff, NULL);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
			table = table->extra_detail.lp_table.next_table;
		} while (LP_LIST_END != table);
		if (YDB_OK != status) {
			return NULL;
		}
	}
	if (NULL != view) {
		/* "view" points to the start of the linked list of LP_VIEW usages in entire query.
		 * Store link between plan and all views that plan uses so a later CREATE/DROP VIEW
		 * of any of the views in this plan knows to delete this stale plan.
		 */
		ydb_buffer_t view_buff[3];

		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWPLANS, &view_buff[0]);
		view_buff[2] = plan_meta[2];
		do {
			assert(LP_VIEW == view->type);

			SqlStatement *view_stmt;
			view_stmt = view->extra_detail.lp_view.table_alias->table;

			SqlView *sql_view;
			UNPACK_SQL_STATEMENT(sql_view, view_stmt, create_view);

			SqlStatement *viewName;
			viewName = sql_view->viewName;
			UNPACK_SQL_STATEMENT(value, viewName, value);
			assert(COLUMN_REFERENCE == value->type);
			YDB_STRING_TO_BUFFER(value->v.string_literal, &view_buff[1]);
			/* Store gvn that links plan and this view */
			status = ydb_set_s(&plan_meta[0], 3, view_buff, NULL);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
			view = view->extra_detail.lp_view.next_view;
		} while (LP_LIST_END != view);
		if (YDB_OK != status) {
			return NULL;
		}
	}
	if (NULL != pplan->outputKey) {
		char col_num_str[INT16_TO_STRING_MAX];

		assert(!IS_INSERT_INTO_PHYSICAL_PLAN(pplan));
		YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_COLUMNS, &plan_meta[3]);
		plan_meta[4].buf_addr = col_num_str;
		plan_meta[4].len_alloc = sizeof(col_num_str);
		plan_meta[4].len_used = 0;

		// Note down column data types
		if (LP_SET_OPERATION == plan->type) {
			LogicalPlan *root_lplan;
			// Find the first query in set operation and use its column data to set the result table's column data
			root_lplan = lp_drill_to_insert(plan);
			cur_plan = lp_get_projection_columns(root_lplan);
		} else {
			cur_plan = pplan->projection;
		}
		do {
			assert(cur_plan->type == LP_COLUMN_LIST);
			GET_LP(column_alias, cur_plan, 0, LP_WHERE);
			GET_LP(column_alias, column_alias, 1, LP_COLUMN_LIST_ALIAS);
			UNPACK_SQL_STATEMENT(value, column_alias->v.lp_column_list_alias.column_list_alias->alias, value);
			// This assumes the SqlValue will outlive this RowDescription
			num_columns++;
			OCTO_INT16_TO_BUFFER(num_columns, &plan_meta[4]);

			YDB_LITERAL_TO_BUFFER(OCTOLIT_NAME, &plan_meta[5]);
			value_buffer.buf_addr = value->v.string_literal;
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				break;

			YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLE_ID, &plan_meta[5]);
			value_buffer.buf_addr = OCTOLIT_0; /* rocto currently uses an OID of 0 for table_id */
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				break;

			YDB_LITERAL_TO_BUFFER(OCTOLIT_COLUMN_ID, &plan_meta[5]);
			value_buffer.buf_addr = OCTOLIT_0; /* rocto currently uses an OID of 0 for column_id */
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				break;

			YDB_LITERAL_TO_BUFFER(OCTOLIT_DATA_TYPE, &plan_meta[5]);
			value_buffer.buf_addr = valbuff;
			value_buffer.len_alloc = INT32_TO_STRING_MAX;
			column_type = get_psql_type_from_sqlvaluetype(column_alias->v.lp_column_list_alias.column_list_alias->type);
			OCTO_INT32_TO_BUFFER(column_type, &value_buffer);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				break;

			YDB_LITERAL_TO_BUFFER(OCTOLIT_DATA_TYPE_SIZE, &plan_meta[5]);
			value_buffer.len_alloc = INT16_TO_STRING_MAX;
			type_size = get_type_size_from_psql_type(column_type);
			// PostgreSQL protocol specifies a 16-bit integer to store each data type size
			// Details linked in rocto/message_formats.h
			OCTO_INT16_TO_BUFFER((int16_t)type_size, &value_buffer);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				break;

			YDB_LITERAL_TO_BUFFER(OCTOLIT_TYPE_MODIFIER, &plan_meta[5]);
			value_buffer.buf_addr = MACRO_STRINGIZE(ROWDESC_DEFAULT_TYPE_MODIFIER);
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				break;

			YDB_LITERAL_TO_BUFFER(OCTOLIT_FORMAT_CODE, &plan_meta[5]);
			value_buffer.buf_addr = MACRO_STRINGIZE(ROWDESC_DEFAULT_FORMAT_CODE);
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				break;
			GET_LP_ALLOW_NULL(cur_plan, cur_plan, 1, LP_COLUMN_LIST);
		} while (NULL != cur_plan);
		if (YDB_OK != status) {
			return NULL;
		}
		// Note down number of output columns for use in RowDescriptions
		value_buffer.buf_addr = valbuff;
		value_buffer.len_alloc = INT16_TO_STRING_MAX;
		OCTO_INT16_TO_BUFFER(num_columns, &value_buffer);
		status = ydb_set_s(plan_meta, 3, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return NULL;
		}
	} else {
		/* Physical plan corresponding to LP_INSERT_INTO/LP_DELETE_FROM/LP_UPDATE has no output columns
		 * so skip "if" block above in that case.
		 */
		assert(IS_INSERT_INTO_PHYSICAL_PLAN(pplan) || IS_DELETE_FROM_PHYSICAL_PLAN(pplan)
		       || IS_UPDATE_PHYSICAL_PLAN(pplan));
	}
	// Create a table from the last physical table which reads from the output values
	return pplan;
}
