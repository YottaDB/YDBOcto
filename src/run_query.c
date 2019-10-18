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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <readline/history.h>


#include <libyottadb.h>
#include <gtmxc_types.h>

// Remove when done debugging
#include <time.h>

#include "mmrhash.h"

#include "octo.h"
#include "octo_types.h"
#include "physical_plan.h"
#include "parser.h"
#include "lexer.h"
#include "helpers.h"

int run_query(int (*callback)(SqlStatement *, int, void*, char*), void *parms) {
	FILE		*out;
	PhysicalPlan	*pplan;
	SqlStatement	*result;
	SqlValue	*value;
	bool		free_memory_chunks;
	char		*buffer, filename[OCTO_PATH_MAX], routine_name[MAX_ROUTINE_LEN];
	char		placeholder;
	gtm_long_t	cursorId;
	hash128_state_t	state;
	int		done, routine_len = MAX_ROUTINE_LEN;
	int		status;
	size_t		buffer_size = 0;
	ydb_buffer_t	*filename_lock = NULL;
	ydb_string_t	ci_filename, ci_routine;
	HIST_ENTRY	*cur_hist;

	memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);

	/* To print only the current query store the index for the last one
	 * then print the difference between the cur_input_index - old_input_index
	 */
	old_input_index = cur_input_index;
	result = parse_line();

	/* add the current query to the readlines history */
	if (config->is_tty) {
		placeholder = input_buffer_combined[cur_input_index];
		input_buffer_combined[cur_input_index] = '\0';
		/* get the last item added to the history
		 * if it is the same as the current query don't add it to thhe history again
		 */
		cur_hist = history_get(history_length);
		if(NULL != cur_hist){
			if (0 != strcmp(cur_hist->line, input_buffer_combined + old_input_index))
				add_history(input_buffer_combined + old_input_index);
		} else {
			add_history(input_buffer_combined + old_input_index);
		}
		input_buffer_combined[cur_input_index] = placeholder;
	}

	INFO(CUSTOM_ERROR, "Parsing done for SQL command [%.*s]", cur_input_index - old_input_index, input_buffer_combined + old_input_index);
	if(result == NULL) {
		INFO(CUSTOM_ERROR, "Returning failure from run_query");
		OCTO_CFREE(memory_chunks);
		return 1;
	}
	if(config->dry_run || (no_data_STATEMENT == result->type)){
		OCTO_CFREE(memory_chunks);
		result = NULL;
		return 0;
	}
	/* Now that we know we are processing a non-empty query, increment the cursor global node */
	ydb_buffer_t	cursor_exe_global;
	ydb_buffer_t	schema_global;

	YDB_MALLOC_BUFFER(&cursor_exe_global, MAX_STR_CONST);
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	status = ydb_incr_s(&schema_global, 0, NULL, NULL, &cursor_exe_global);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&cursor_exe_global);
		OCTO_CFREE(memory_chunks);
		return 1;
	}
	cursor_exe_global.buf_addr[cursor_exe_global.len_used] = '\0';
	INFO(CUSTOM_ERROR, "Generating SQL for cursor %s", cursor_exe_global.buf_addr);
	free_memory_chunks = true;	// By default run "octo_cfree(memory_chunks)" at the end

	// These are used in the drop and create table cases, but we can't put them closer
	// to their uses because of the way C handles scope
	ydb_buffer_t	table_name_buffers[3];
	ydb_buffer_t	*table_name_buffer = &table_name_buffers[0],
			*table_type_buffer = &table_name_buffers[1],
			*table_sub_buffer = &table_name_buffers[2];
	ydb_buffer_t	table_binary_buffer, table_create_buffer;

	switch(result->type) {
	// This effectively means select_STATEMENT, but we have to assign ID's inside this function
	// and so need to propagate them out
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		TRACE(ERR_ENTERING_FUNCTION, "hash_canonical_query");
		INVOKE_HASH_CANONICAL_QUERY(state, result, status);	/* "state" holds final hash */
		if (0 != status) {
			YDB_FREE_BUFFER(&cursor_exe_global);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		status = generate_routine_name(&state, routine_name, routine_len, OutputPlan);
		if (1 == status) {
			ERROR(ERR_PLAN_HASH_FAILED, "");
			YDB_FREE_BUFFER(&cursor_exe_global);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		GET_FULL_PATH_OF_GENERATED_M_FILE(filename, &routine_name[1]);	/* updates "filename" to be full path */
		if (access(filename, F_OK) == -1) {	// file doesn't exist
			INFO(CUSTOM_ERROR, "Generating M file [%s] (to execute SQL query)", filename);
			filename_lock = make_buffers("^%ydboctoocto", 2, "files", filename);
			// Wait for 5 seconds in case another process is writing to same filename
			status = ydb_lock_incr_s(5000000000, &filename_lock[0], 2, &filename_lock[1]);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				YDB_FREE_BUFFER(&cursor_exe_global);
				OCTO_CFREE(memory_chunks);
				free(filename_lock);
				return 1;
			}
			if (access(filename, F_OK) == -1) {
				pplan = emit_select_statement(result, filename);
				if(pplan == NULL) {
					YDB_FREE_BUFFER(&cursor_exe_global);
					OCTO_CFREE(memory_chunks);
					ydb_lock_decr_s(&filename_lock[0], 2, &filename_lock[1]);
					free(filename_lock);
					result = NULL;
					return 1;
				}
				assert(pplan != NULL);
			}
			status = ydb_lock_decr_s(&filename_lock[0], 2, &filename_lock[1]);
			free(filename_lock);
			if (YDB_OK != status) {
				YDB_FREE_BUFFER(&cursor_exe_global);
				OCTO_CFREE(memory_chunks);
				return 1;
			}
		} else {
			INFO(CUSTOM_ERROR, "Using already generated M file [%s] (to execute SQL query)", filename);
		}
		cursorId = atol(cursor_exe_global.buf_addr);
		ci_filename.address = filename;
		ci_filename.length = strlen(filename);
		ci_routine.address = routine_name;
		ci_routine.length = routine_len;
		// Call the select routine
		status = ydb_ci("_ydboctoselect", cursorId, &ci_filename, &ci_routine);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&cursor_exe_global);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		// Check for cancel requests only if running rocto
		if (config->is_rocto) {
			// Check if execution was interrupted by a CancelRequest by checking local variable
			ydb_buffer_t ydboctoCancel;
			unsigned int cancel_result = 0;
			YDB_LITERAL_TO_BUFFER("%ydboctoCancel", &ydboctoCancel);
			status = ydb_data_s(&ydboctoCancel, 0, NULL, &cancel_result);
			YDB_ERROR_CHECK(status);
			if (0 != cancel_result) {
				// Omit results after handling CancelRequest
				YDB_FREE_BUFFER(&cursor_exe_global);
				OCTO_CFREE(memory_chunks);
				status = (*callback)(NULL, cursorId, parms, filename);
				if (0 != status) {
					// This should never happen
					assert(0 == status);
					FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
					return 1;
				}
				return -1;
			}
		}
		status = (*callback)(result, cursorId, parms, filename);
		if (0 != status) {
			YDB_FREE_BUFFER(&cursor_exe_global);
			// May be freed in the callback function, must check before freeing
			if (NULL != memory_chunks) {
				OCTO_CFREE(memory_chunks);
			}
			return 1;
		}
		// Deciding to free the select_STATEMENT must be done by the caller, as they may want to rerun it or send row
		// descriptions hence the decision to not free the memory_chunk below.
		free_memory_chunks = false;
		break;
	case table_STATEMENT:
		out = open_memstream(&buffer, &buffer_size);
		assert(out);
		status = emit_create_table(out, result);
		fclose(out);	// at this point "buffer" and "buffer_size" are usable
		if (0 != status) {
			free(buffer);
			YDB_FREE_BUFFER(&cursor_exe_global);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		INFO(CUSTOM_ERROR, "%s", buffer);

		YDB_MALLOC_BUFFER(table_name_buffer, MAX_STR_CONST);
		YDB_MALLOC_BUFFER(table_sub_buffer, MAX_STR_CONST);
		SqlTable *table;
		UNPACK_SQL_STATEMENT(table, result, table);
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		YDB_COPY_STRING_TO_BUFFER(value->v.reference, table_name_buffer, done);
		if(!done) {
			ERROR(ERR_TABLE_DEFINITION_TOO_LONG, value->v.reference,
					table_name_buffer->len_alloc,
					strlen(value->v.reference));
			free(buffer);
			YDB_FREE_BUFFER(&cursor_exe_global);
			YDB_FREE_BUFFER(table_name_buffer);
			YDB_FREE_BUFFER(table_sub_buffer);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		YDB_STRING_TO_BUFFER(buffer, &table_create_buffer);
		YDB_STRING_TO_BUFFER("t", table_type_buffer);
		status = ydb_set_s(&schema_global, 2,
				   table_name_buffers,
				   &table_create_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			free(buffer);
			YDB_FREE_BUFFER(&cursor_exe_global);
			YDB_FREE_BUFFER(table_name_buffer);
			YDB_FREE_BUFFER(table_sub_buffer);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		free(buffer);	// Note that "table_create_buffer" (whose "buf_addr" points to "buffer") is also no longer usable
		char *table_buffer = NULL;
		int length;
		compress_statement(result, &table_buffer, &length);
		assert(table_buffer != NULL);
		int cur_length = 0;
		int i = 0;
		table_binary_buffer.len_alloc = MAX_STR_CONST;
		YDB_STRING_TO_BUFFER("b", table_type_buffer);
		while(cur_length < length) {
			table_sub_buffer->len_used = snprintf(table_sub_buffer->buf_addr, table_sub_buffer->len_alloc, "%d", i);
			table_binary_buffer.buf_addr = &table_buffer[cur_length];
			if(MAX_STR_CONST < length - cur_length) {
				table_binary_buffer.len_used = MAX_STR_CONST;
			} else {
				table_binary_buffer.len_used = length - cur_length;
			}
			status = ydb_set_s(&schema_global, 3,
				   table_name_buffers,
				   &table_binary_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				free(table_buffer);
				YDB_FREE_BUFFER(&cursor_exe_global);
				YDB_FREE_BUFFER(table_name_buffer);
				YDB_FREE_BUFFER(table_sub_buffer);
				OCTO_CFREE(memory_chunks);
				return 1;
			}
			cur_length += MAX_STR_CONST;
			i++;
		}
		YDB_STRING_TO_BUFFER("l", table_type_buffer);
		// Use table_sub_buffer as a temporary buffer below
		table_sub_buffer->len_used = snprintf(table_sub_buffer->buf_addr, table_sub_buffer->len_alloc, "%d", length);
		status = ydb_set_s(&schema_global, 2, table_name_buffers, table_sub_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			free(table_buffer);
			YDB_FREE_BUFFER(&cursor_exe_global);
			YDB_FREE_BUFFER(table_name_buffer);
			YDB_FREE_BUFFER(table_sub_buffer);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		status = store_table_in_pg_class(table);
		if (YDB_OK != status) {
			free(table_buffer);
			YDB_FREE_BUFFER(&cursor_exe_global);
			YDB_FREE_BUFFER(table_name_buffer);
			YDB_FREE_BUFFER(table_sub_buffer);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		free(table_buffer);
		// Drop the table from the local cache
		YDB_LITERAL_TO_BUFFER("%ydboctoloadedschemas", &schema_global);
		status = ydb_delete_s(&schema_global, 1,
				table_name_buffer,
				YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			free(table_buffer);
			YDB_FREE_BUFFER(&cursor_exe_global);
			YDB_FREE_BUFFER(table_name_buffer);
			YDB_FREE_BUFFER(table_sub_buffer);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		YDB_FREE_BUFFER(table_name_buffer);
		YDB_FREE_BUFFER(table_sub_buffer);
		break;
	case drop_STATEMENT:
		YDB_MALLOC_BUFFER(table_name_buffer, MAX_STR_CONST);
		YDB_COPY_STRING_TO_BUFFER(result->v.drop->table_name->v.value->v.reference, table_name_buffer, done);
		status = ydb_delete_s(&schema_global, 1,
				      table_name_buffer,
				      YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&cursor_exe_global);
			YDB_FREE_BUFFER(table_name_buffer);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		// Drop the table from the local cache
		YDB_LITERAL_TO_BUFFER("%ydboctoloadedschemas", &schema_global);
		status = ydb_delete_s(&schema_global, 1,
				table_name_buffer,
				YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&cursor_exe_global);
			YDB_FREE_BUFFER(table_name_buffer);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		YDB_FREE_BUFFER(table_name_buffer);
		break;
	case insert_STATEMENT:
		WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "table inserts");
		break;
	case begin_STATEMENT:
	case commit_STATEMENT:
		WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "transactions");
		break;
	case set_STATEMENT:
	case show_STATEMENT:
		cursorId = atol(cursor_exe_global.buf_addr);
		(*callback)(result, cursorId, parms, NULL);
		break;
	default:
		WARNING(ERR_FEATURE_NOT_IMPLEMENTED, input_buffer_combined);
	}
	YDB_FREE_BUFFER(&cursor_exe_global);
	if (free_memory_chunks) {
		OCTO_CFREE(memory_chunks);
	}
	result = NULL;
	return 0;
}
