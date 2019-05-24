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
	SqlColumn *column;
	SqlValue *value;
	LogicalPlan *plan, *cur_plan, *column_alias;
	PhysicalPlan *pplan;
	char output_key[MAX_STR_CONST], column_id_buffer[MAX_STR_CONST];
	char buffer[MAX_STR_CONST];
	ydb_buffer_t *plan_meta, value_buffer;
	ydb_buffer_t z_status, z_status_value;

	TRACE(ERR_ENTERING_FUNCTION, "emit_select_statement");
	memset(output_key, 0, MAX_STR_CONST);

	assert(stmt && stmt->type == table_alias_STATEMENT);
	plan = generate_logical_plan(stmt, &config->plan_id);
	if(lp_verify_structure(plan) == FALSE)
		FATAL(ERR_PLAN_NOT_WELL_FORMED, "");
	if(config->record_error_level <= DEBUG) {
		lp_emit_plan(buffer, MAX_STR_CONST, plan);
		DEBUG(ERR_CURPLAN, buffer);
	}
	optimize_logical_plan(plan);
	if(config->record_error_level <= DEBUG) {
		lp_emit_plan(buffer, MAX_STR_CONST, plan);
		DEBUG(ERR_CURPLAN, buffer);
	}
	pplan = generate_physical_plan(plan, NULL);
	if (plan_filename) {
		emit_physical_plan(pplan, plan_filename);
	}
	while(pplan->next != NULL)
		pplan = pplan->next;

	// convert output key to string
	snprintf(output_key, MAX_STR_CONST, "%d", pplan->outputKey->random_id);
	if(plan_filename) {
		set(output_key, config->global_names.octo, 3, "plan_metadata", plan_filename, "output_key");
		plan_meta = make_buffers(config->global_names.octo, 5, "plan_metadata", plan_filename,
				"output_columns", "", "");
		plan_meta[4].len_alloc = MAX_STR_CONST;
		plan_meta[4].buf_addr = column_id_buffer;

		// Note down column data types
		int num_columns = 0, status = 0;
		cur_plan = pplan->projection;
		do {
			assert(cur_plan->type == LP_COLUMN_LIST);
			GET_LP(column_alias, cur_plan, 0, LP_WHERE);
			if(column_alias->v.operand[1] != NULL) {
				GET_LP(column_alias, column_alias, 1, LP_COLUMN_LIST_ALIAS);
				UNPACK_SQL_STATEMENT(value, column_alias->v.column_list_alias->alias, value);
				// This assumes the SqlValue will outlive this RowDescription
			} else {
				GET_LP(column_alias, column_alias, 0, LP_COLUMN_ALIAS);
				UNPACK_SQL_STATEMENT(column, column_alias->v.column_alias->column, column);
				UNPACK_SQL_STATEMENT(value, column->columnName, value);
			}
			num_columns++;
			plan_meta[4].len_used = snprintf(column_id_buffer, MAX_STR_CONST, "%d", num_columns);

			YDB_LITERAL_TO_BUFFER("name", &plan_meta[5]);
			value_buffer.buf_addr = value->v.string_literal;
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);

			YDB_LITERAL_TO_BUFFER("table_id", &plan_meta[5]);
			value_buffer.buf_addr = "0";
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);

			YDB_LITERAL_TO_BUFFER("column_id", &plan_meta[5]);
			value_buffer.buf_addr = "0";
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);

			YDB_LITERAL_TO_BUFFER("data_type", &plan_meta[5]);
			value_buffer.buf_addr = "25";
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);

			YDB_LITERAL_TO_BUFFER("data_type_size", &plan_meta[5]);
			value_buffer.buf_addr = "-1";
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);

			YDB_LITERAL_TO_BUFFER("type_modifier", &plan_meta[5]);
			value_buffer.buf_addr = "-1";
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);

			YDB_LITERAL_TO_BUFFER("format_code", &plan_meta[5]);
			value_buffer.buf_addr = "0";
			value_buffer.len_used = value_buffer.len_alloc = strlen(value_buffer.buf_addr);
			status = ydb_set_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);
			cur_plan = cur_plan->v.operand[1];
		} while(cur_plan != NULL);

		free(plan_meta);
	}

	// Create a table from the last physical table which reads from the output
	//  values
	return pplan;
}
