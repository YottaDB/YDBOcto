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

/**
 * Emits M code for retrieving values representing this SELECT statement
 *
 * @param cursor_exe_global an array of size 3
 * Returns a table describing the temporary table containing the resulting
 *  values
 */
SqlTable *emit_select_statement(ydb_buffer_t *cursor_global,
                                ydb_buffer_t *cursor_exe_global, SqlStatement *stmt,
                                SqlTable *destination_table)
{
	FILE *output, *temp_table;
	SqlSelectStatement *select;
	SqlColumnList *cur_column_list, *start_column_list, *new_column_list, *t_column_list;
	SqlStatement *tmp_statement;
	SqlTable *table = NULL;
	SqlColumn *cur_column, *start_column;
	SqlStatement *result = 0;
	SqlJoin *join;
	char *temp_table_buffer, *output_buffer;
	size_t temp_table_buffer_size = 0, output_buffer_size = 0;
	char *tmp1, *formatted_start, *start, *end, *curse, *source;
	char temp_table_name[MAX_STR_CONST], temp_cursor_name[MAX_STR_CONST], buffer[MAX_STR_CONST];
	int column_name_length, table_name_length, column_counter = 0, status, temporary_table = 0, max_key = 0;
	int optimizations = 0, len = 0;
	ydb_buffer_t schema_global, latest_schema_id;
	ydb_buffer_t m_exe_buffer_value;
	ydb_buffer_t z_status, z_status_value;

	TRACE(ERR_ENTERING_FUNCTION, "emit_select_statement")

	// Create a temporary table
	temp_table = open_memstream(&temp_table_buffer, &temp_table_buffer_size);
	YDB_LITERAL_TO_BUFFER("^schema", &schema_global);
	INIT_YDB_BUFFER(&latest_schema_id, MAX_STR_CONST);
	status = ydb_incr_s(&schema_global, 0, NULL, NULL, &latest_schema_id);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	latest_schema_id.buf_addr[latest_schema_id.len_used] = '\0';
	snprintf(temp_table_name, MAX_STR_CONST, "tempTbl%s", latest_schema_id.buf_addr);
	fprintf(temp_table, TEMPLATE_CREATE_TABLE_START, temp_table_name);

	output = open_memstream(&output_buffer, &output_buffer_size);

	//fprintf(output, " WRITE ");
	/// TODO: we should copy all tables from the join so we don't screw them up
	assert(stmt && stmt->type == select_STATEMENT);
	UNPACK_SQL_STATEMENT(select, stmt, select);
	optimizations = optimize_where(select->where_expression, join);
	UNPACK_SQL_STATEMENT(join, select->table_list, join);
	switch(join->type) {
	case NO_JOIN:
		UNPACK_SQL_STATEMENT(table, join->value, table);
		break;
	case INNER_JOIN:
		//create_temporary_join_table();
		break;
	case TABLE_SPEC:
		//UNPACK_SQL_STATEMENT(value, join->value, value);
		temporary_table = TRUE;
		table = emit_select_statement(cursor_global, cursor_exe_global, join->value, NULL);
		break;
	}
	assert(table != NULL);
	get_table_parts(table, &curse, &start, &end, &source);

	// If there was an optimization, recalculate the cursor and end condition
	if(optimizations > 0) {
		table->curse = NULL;
		generate_cursor(buffer, MAX_STR_CONST, table);
		free(curse);
		len = strlen(buffer);
		curse = malloc(len + 1);
		memcpy(curse, buffer, len);
		curse[len] = '\0';

		table->end = NULL;
		generate_end(buffer, MAX_STR_CONST, table);
		free(end);
		len = strlen(buffer);
		end = malloc(len + 1);
		memcpy(end, buffer, len);
		end[len] = '\0';
	}

	formatted_start = malloc(MAX_STR_CONST);
	snprintf(temp_cursor_name, MAX_STR_CONST, "^cursor(%s)", cursor_exe_global[0].buf_addr);
	snprintf(formatted_start, MAX_STR_CONST, start, temp_cursor_name);
	fprintf(output, TEMPLATE_SELECT_BASIC, formatted_start, curse, end, end, end);

	if(generate_null_check(buffer, MAX_STR_CONST, table, -1) == 1)
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
	fprintf(output, "SET:'(%s)", buffer);
	UNPACK_SQL_STATEMENT(start_column_list, select->select_list, column_list);
	if(select->where_expression) {
		tmp1 = extract_expression(cursor_global, cursor_exe_global, select->where_expression, table, source);
		fprintf(output, "&(%s)", tmp1);
	}
	fprintf(output, " ^%s(rowId)=\"|\"", temp_table_name);
	if (start_column_list == NULL) {
		/* This was a SELECT * statement; add all columns to a list */
		UNPACK_SQL_STATEMENT(start_column, table->columns, column);
		cur_column = start_column;
		// We can't use MALLOC_STATEMENT here because it's not wrapped in a SqlStatement
		start_column_list = (SqlColumnList*)malloc(sizeof(SqlColumnList));
		memset(start_column_list, 0, sizeof(SqlColumnList));
		dqinit(start_column_list);
		cur_column_list = start_column_list;
		table_name_length = strnlen(table->tableName->v.value->v.reference, MAX_STR_CONST);
		if(temporary_table)
			cur_column = cur_column->next;
		do {
			SQL_STATEMENT(tmp_statement, value_STATEMENT);
			MALLOC_STATEMENT(tmp_statement, value, SqlValue);
			tmp_statement->v.value->type = COLUMN_REFERENCE;
			column_name_length = strnlen(cur_column->columnName->v.value->v.reference, MAX_STR_CONST);
			tmp_statement->v.value->v.reference = malloc(column_name_length + table_name_length + 3);
			strncpy(tmp_statement->v.value->v.reference, table->tableName->v.value->v.reference, table_name_length);
			*(tmp_statement->v.value->v.reference + table_name_length) = '.';
			strncpy(tmp_statement->v.value->v.reference + table_name_length + 1,
			        cur_column->columnName->v.value->v.reference, column_name_length + 2);
			cur_column_list->value = tmp_statement;
			cur_column = cur_column->next;
			if(cur_column != start_column) {
				new_column_list = (SqlColumnList*)malloc(sizeof(SqlColumnList));
				memset(new_column_list, 0, sizeof(SqlColumnList));
				dqinit(new_column_list);
				dqinsert(start_column_list, new_column_list, t_column_list);
				cur_column_list = new_column_list;
			}
		} while(cur_column != start_column);
	}
	cur_column_list = start_column_list;
	do {
		fprintf(temp_table, TEMPLATE_CREATE_TABLE_COLUMN, column_counter);
		column_counter++;
		tmp1 = extract_expression(cursor_global, cursor_exe_global, cur_column_list->value, table, source);
		fprintf(output, "_%s_\"|\"", tmp1);
		free(tmp1);
		cur_column_list = cur_column_list->next;
	} while(cur_column_list != start_column_list);

	fprintf(temp_table, ");");
	fclose(temp_table);
	INFO(ERR_GENERATING_TEMPORARY_TABLE, temp_table_buffer)
	result = parse_line(temp_table_buffer);
	UNPACK_SQL_STATEMENT(table, result, table);
	free(formatted_start);
	free(start);
	free(end);
	free(curse);
	free(source);
	fclose(output);
	status = ydb_incr_s(cursor_global, 2, cursor_exe_global, NULL, &cursor_exe_global[2]);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	cursor_exe_global[2].buf_addr[cursor_exe_global[2].len_used] = '\0';
	INFO(ERR_ADDING_EXE, cursor_exe_global[2].buf_addr, output_buffer);
	m_exe_buffer_value.buf_addr = output_buffer;
	m_exe_buffer_value.len_used = m_exe_buffer_value.len_alloc = output_buffer_size;
	status = ydb_set_s(cursor_global, 3,
	                   cursor_exe_global,
	                   &m_exe_buffer_value);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	TRACE(ERR_LEAVING_FUNCTION, "emit_select_statement");
	free(output_buffer);
	return table;
}
