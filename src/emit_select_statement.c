/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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
#include "helpers.h"

/**
 * Emits M code for retrieving values representing this SELECT statement
 *
 * @param cursor_exe_global an array of size 3
 * Returns a table describing the temporary table containing the resulting
 *  values
 */
PhysicalPlan *emit_select_statement(SqlStatement *stmt, char *plan_filename)
{
	LPActionType		set_oper_type;
	LogicalPlan		*plan, *cur_plan, *column_alias;
	PhysicalPlan		*pplan;
	SqlValue		*value;
	char			output_key[MAX_STR_CONST], column_id_buffer[MAX_STR_CONST];
	int			output_key_id, status = 0;
	ydb_buffer_t		*plan_meta, value_buffer;
	SetOperType		*set_oper;
	PhysicalPlanOptions	options;

	TRACE(ERR_ENTERING_FUNCTION, "emit_select_statement");
	memset(output_key, 0, MAX_STR_CONST);

	assert(stmt && ((table_alias_STATEMENT == stmt->type) || (set_operation_STATEMENT == stmt->type)));
	plan = generate_logical_plan(stmt, &config->plan_id);
	if (NULL == plan) {
		return NULL;
	}
	lp_emit_plan(plan, "BEFORE optimize_logical_plan()");
	if (lp_verify_structure(plan, NULL) == FALSE) {
		ERROR(ERR_PLAN_NOT_WELL_FORMED, "");
		return NULL;
	}
	plan = optimize_logical_plan(plan);
	if (NULL == plan) {
		ERROR(ERR_FAILED_TO_OPTIMIZE_PLAN, "");
		return NULL;
	}
	lp_emit_plan(plan, "AFTER optimize_logical_plan()");
	memset(&options, 0, sizeof(PhysicalPlanOptions));
	pplan = NULL;
	options.last_plan = &pplan;
	pplan = generate_physical_plan(plan, &options);
	if (NULL == pplan) {
		ERROR(ERR_PLAN_NOT_GENERATED, "physical");
		return NULL;
	}
	assert(NULL != plan_filename);
	status = emit_physical_plan(pplan, plan_filename);
	if (YDB_OK != status)
		return NULL;

	set_oper = pplan->set_oper_list;
	set_oper_type = ((NULL == set_oper) ? LP_INVALID_ACTION : set_oper->set_oper_type);
	assert(((LP_INVALID_ACTION == set_oper_type) && !set_oper_type)
		|| (LP_SET_UNION == set_oper_type) || (LP_SET_UNION_ALL == set_oper_type) || (LP_SET_DNF == set_oper_type)
		|| (LP_SET_EXCEPT == set_oper_type) || (LP_SET_EXCEPT_ALL == set_oper_type)
		|| (LP_SET_INTERSECT == set_oper_type) || (LP_SET_INTERSECT_ALL == set_oper_type));
	output_key_id = (set_oper_type ? set_oper->output_id : pplan->outputKey->unique_id);
	// convert output key to string
	snprintf(output_key, MAX_STR_CONST, "%d", output_key_id);
	set(output_key, config->global_names.octo, 3, "plan_metadata", plan_filename, "output_key");
	plan_meta = make_buffers(config->global_names.octo, 5, "plan_metadata", plan_filename,
			"output_columns", "", "");
	plan_meta[4].len_alloc = MAX_STR_CONST;
	plan_meta[4].buf_addr = column_id_buffer;

	// Note down column data types
	int num_columns = 0;
	cur_plan = pplan->projection;
	do {
		assert(cur_plan->type == LP_COLUMN_LIST);
		GET_LP(column_alias, cur_plan, 0, LP_WHERE);
		assert(NULL != column_alias->v.lp_default.operand[1]);
		GET_LP(column_alias, column_alias, 1, LP_COLUMN_LIST_ALIAS);
		UNPACK_SQL_STATEMENT(value, column_alias->v.lp_column_list_alias.column_list_alias->alias, value);
		// This assumes the SqlValue will outlive this RowDescription
		num_columns++;
		plan_meta[4].len_used = snprintf(column_id_buffer, MAX_STR_CONST, "%d", num_columns);

		YDB_LITERAL_TO_BUFFER("name", &plan_meta[5]);
		value_buffer.buf_addr = value->v.string_literal;
		value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
		status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;

		YDB_LITERAL_TO_BUFFER("table_id", &plan_meta[5]);
		value_buffer.buf_addr = "0";
		value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
		status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;

		YDB_LITERAL_TO_BUFFER("column_id", &plan_meta[5]);
		value_buffer.buf_addr = "0";
		value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
		status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;

		YDB_LITERAL_TO_BUFFER("data_type", &plan_meta[5]);
		YDB_MALLOC_BUFFER(&value_buffer, INT16_TO_STRING_MAX);
		value_buffer.len_used = snprintf(value_buffer.buf_addr, INT16_TO_STRING_MAX, "%d", PSQL_TypeOid_varchar);
		status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_FREE_BUFFER(&value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;

		YDB_LITERAL_TO_BUFFER("data_type_size", &plan_meta[5]);
		value_buffer.buf_addr = "-1";
		value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
		status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;

		YDB_LITERAL_TO_BUFFER("type_modifier", &plan_meta[5]);
		value_buffer.buf_addr = "-1";
		value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
		status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;

		YDB_LITERAL_TO_BUFFER("format_code", &plan_meta[5]);
		value_buffer.buf_addr = "0";
		value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
		status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		cur_plan = cur_plan->v.lp_default.operand[1];
	} while (NULL != cur_plan);
	free(plan_meta);
	if (YDB_OK != status)
		return NULL;
	// Create a table from the last physical table which reads from the output values
	return pplan;
}
