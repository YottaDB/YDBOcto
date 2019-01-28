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

/**
 * Emits M code for retrieving values representing this SELECT statement
 *
 * @param cursor_exe_global an array of size 3
 * Returns a table describing the temporary table containing the resulting
 *  values
 */
PhysicalPlan *emit_select_statement(ydb_buffer_t *cursor_global,
                                ydb_buffer_t *cursor_exe_global, SqlStatement *stmt,
                                SqlTable *destination_table)
{
	FILE *output, *temp_table;
	SqlSelectStatement *select;
	SqlColumnList *cur_column_list, *start_column_list, *new_column_list, *t_column_list;
	SqlStatement *tmp_statement;
	SqlTable *table = NULL, *join_tables[2];
	SqlColumn *cur_column, *start_column;
	SqlStatement *result = 0;
	SqlJoin *join, *start_join, *cur_join;
	SqlValue *value;
	LogicalPlan *plan;
	PhysicalPlan *pplan;
	char *temp_table_buffer, *output_buffer;
	size_t temp_table_buffer_size = 0, output_buffer_size = 0;
	char *tmp1, *formatted_start, *start, *end, *curse, *source;
	char temp_table_name[MAX_STR_CONST], temp_cursor_name[MAX_STR_CONST], buffer[MAX_STR_CONST];
	int column_name_length, table_name_length, column_counter = 0, status, temporary_table = 0, max_key = 0;
	int optimizations = 0, len = 0, i = 0;
	ydb_buffer_t schema_global, latest_schema_id;
	ydb_buffer_t m_exe_buffer_value;
	ydb_buffer_t z_status, z_status_value;

	TRACE(ERR_ENTERING_FUNCTION, "emit_select_statement");

	assert(stmt && stmt->type == select_STATEMENT);
	UNPACK_SQL_STATEMENT(select, stmt, select);
	plan = generate_logical_plan(stmt, &config->plan_id);
	if(lp_verify_structure(plan) == FALSE)
		FATAL(ERR_PLAN_NOT_WELL_FORMED);
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
	emit_physical_plan(pplan);

	// Create a table from the last physical table which reads from the output
	//  values
	return pplan;
}
