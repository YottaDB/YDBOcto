/* Copyright (C) 2018-2019 YottaDB, LLC
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
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"
#include "helpers.h"

int handle_describe(Describe *describe, RoctoSession *session) {
	ydb_buffer_t subs_array[3], dest_subs[3];
	ydb_buffer_t session_global, sql_expression, *source_name = &subs_array[2], *prepared = &subs_array[1], *source_session_id = &subs_array[0];
	ydb_buffer_t *dest_session_id = &dest_subs[0], *bound = &dest_subs[1], *parse_name = &dest_subs[2];
	ydb_buffer_t *val_buff, empty_buffer;
	ydb_buffer_t plan_name_b;
	ydb_buffer_t z_status, z_status_value;
	size_t query_length = 0, err_buff_size;
	int done = FALSE, length, status, parse_parm, found = 0;
	char *ptr, *end_ptr, new_query[MAX_STR_CONST];
	char *int_start, *new_query_ptr, *end_new_query_ptr;
	char *new_value_start, *new_value_end, c;
	char *err_buff;
	ydb_buffer_t schema_global, table_name_buffer, table_create_buffer, null_buffer;
	ydb_buffer_t cursor_global, cursor_exe_global[3];
	PhysicalPlan *pplan;
	SqlStatement *statement;
	ParseComplete *response;
	ErrorResponse *err;
	RowDescription *description;
	RowDescriptionParm row_desc_parm;
	SqlSetStatement *set_stmt;
	SqlShowStatement *show_stmt;
	SqlValue *val1, *val2;
	NoData *no_data;
	hash128_state_t state;
	char filename[MAX_STR_CONST];
	int filename_len = 0;
	ydb_buffer_t *filename_lock = NULL;

	// zstatus buffers
	YDB_LITERAL_TO_BUFFER("$ZSTATUS", &z_status);
	INIT_YDB_BUFFER(&z_status_value, MAX_STR_CONST);
	YDB_LITERAL_TO_BUFFER("", &empty_buffer);

	// Fetch the named SQL query from the session ^session(id, "prepared", <name>)
	YDB_STRING_TO_BUFFER(config->global_names.session, &session_global);
	INIT_YDB_BUFFER(source_session_id, session->session_id->len_used);
	YDB_COPY_BUFFER_TO_BUFFER(session->session_id, source_session_id, done);
	assert(done == TRUE);
	if(describe->item == 'S') {
		YDB_LITERAL_TO_BUFFER("prepared", prepared);
	} else {
		YDB_LITERAL_TO_BUFFER("bound", prepared);
	}
	source_name->buf_addr = describe->name;
	source_name->len_used = source_name->len_alloc = strlen(describe->name);

	INIT_YDB_BUFFER(&sql_expression, MAX_STR_CONST);

	// Check if portal exists
	status = ydb_data_s(&session_global, 3, subs_array, &found);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	if(found == 0) {
		/// TODO: return error here
	}

	status = ydb_get_s(&session_global, 3, subs_array, &sql_expression);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	query_length = sql_expression.len_used;
	memcpy(input_buffer_combined, sql_expression.buf_addr, query_length);
	/*if(input_buffer_combined[query_length-1] != ';' ) {
		input_buffer_combined[query_length++] = ';';
	}*/
	eof_hit = FALSE;
	input_buffer_combined[query_length] = '\0';
	cur_input_index = 0;
	cur_input_more = &no_more;
	//err_buffer = stderr;
	err_buffer = open_memstream(&err_buff, &err_buff_size);
	do {
		memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
		statement = parse_line(input_buffer_combined);
		if(statement == NULL) {
			fflush(err_buffer);
			fclose(err_buffer);
			err = make_error_response(PSQL_Error_ERROR,
					PSQL_Code_Syntax_Error,
					err_buff,
					0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
			free(err_buff);
			err_buffer = open_memstream(&err_buff, &err_buff_size);
			octo_cfree(memory_chunks);
			continue;
		}
		// Else, send back the row description
		// Allocate items to create the cursor_exe_global
		YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
		YDB_LITERAL_TO_BUFFER("", &null_buffer);
		YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_global);
		INIT_YDB_BUFFER(&cursor_exe_global[0], MAX_STR_CONST);
		YDB_LITERAL_TO_BUFFER("exe", &cursor_exe_global[1]);
		INIT_YDB_BUFFER(&cursor_exe_global[2], MAX_STR_CONST);

		status = ydb_incr_s(&schema_global, 0, NULL, NULL, &cursor_exe_global[0]);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		cursor_exe_global[0].buf_addr[cursor_exe_global[0].len_used] = '\0';
		cursor_exe_global[2].len_used = 1;
		*cursor_exe_global[2].buf_addr = '0';

		switch(statement->type) {
			case select_STATEMENT:
				HASH128_STATE_INIT(state, 0);
				hash_canonical_query(&state, statement);
				filename_len = generate_filename(&state, config->tmp_dir, filename, OutputPlan);
				if (filename_len < 0) {
					FATAL(ERR_PLAN_HASH_FAILED);
				}
				if (access(filename, F_OK) == -1) {	// file doesn't exist
					filename_lock = make_buffers("^%ydboctoocto", 2, "files", filename);
					// Wait for 5 seconds in case another process is writing to same filename
					ydb_lock_incr_s(5000000000, &filename_lock[0], 2, &filename_lock[1]);
					if (access(filename, F_OK) == -1) {
						pplan = emit_select_statement(&cursor_global, cursor_exe_global, statement, filename, NULL);
						assert(pplan != NULL);
					}
					ydb_lock_decr_s(&filename_lock[0], 2, &filename_lock[1]);
					free(filename_lock);
				}
				plan_name_b.buf_addr = filename;
				plan_name_b.len_alloc = plan_name_b.len_used = strlen(filename);
				description = get_plan_row_description(&plan_name_b);
				send_message(session, (BaseMessage*)(&description->type));
				free(description);
				break;
			case no_data_STATEMENT:
				no_data = make_no_data();
				send_message(session, (BaseMessage*)(&no_data->type));
				free(no_data);
				break;
			default:
				description = make_row_description(NULL, 0);
				send_message(session, (BaseMessage*)(&description->type));
				free(description);
		}
		octo_cfree(memory_chunks);
	} while(!eof_hit);

	return 0;
}
