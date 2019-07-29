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

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "mmrhash.h"

#include "octo.h"
#include "octo_types.h"
#include "physical_plan.h"
#include "parser.h"
#include "lexer.h"
#include "helpers.h"

int run_query(char *query, void (*callback)(SqlStatement *, int, void*, char*), void *parms) {
	FILE		*out;
	PhysicalPlan	*pplan;
	SqlStatement	*result;
	SqlValue	*value;
	bool		free_memory_chunks;
	char		*buffer, filename[OCTO_PATH_MAX], routine_name[MAX_ROUTINE_LEN];
	gtm_long_t	cursorId;
	hash128_state_t	state;
	int		done, routine_len = 0;
	int		status;
	size_t		buffer_size = 0;
	ydb_buffer_t	*filename_lock = NULL;
	ydb_string_t	ci_filename, ci_routine;

	memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);

	result = parse_line(query);
	INFO(CUSTOM_ERROR, "Parsing done for SQL command [%s]", query);
	if(result == NULL) {
		INFO(CUSTOM_ERROR, "Returning failure from run_query");
		octo_cfree(memory_chunks);
		return 0;
	}
	if(config->dry_run || (no_data_STATEMENT == result->type)){
		octo_cfree(memory_chunks);
		result = NULL;
		return 1;
	}
	/* Now that we know we are processing a non-empty query, increment the cursor global node */
	ydb_buffer_t	cursor_exe_global;
	ydb_buffer_t	schema_global;

	YDB_MALLOC_BUFFER(&cursor_exe_global, MAX_STR_CONST);
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	status = ydb_incr_s(&schema_global, 0, NULL, NULL, &cursor_exe_global);
	YDB_ERROR_CHECK(status);
	cursor_exe_global.buf_addr[cursor_exe_global.len_used] = '\0';
	INFO(CUSTOM_ERROR, "Generating SQL for cursor %s", cursor_exe_global.buf_addr);
	free_memory_chunks = true;	// By default run "octo_cfree(memory_chunks)" at the end

	// These are used in the drop and create table cases, but we can't put them closer
	// to their uses because of the way C handles scope
	ydb_buffer_t table_name_buffers[3];
	ydb_buffer_t *table_name_buffer = &table_name_buffers[0],
	*table_type_buffer = &table_name_buffers[1],
	*table_sub_buffer = &table_name_buffers[2];
	ydb_buffer_t table_binary_buffer, table_create_buffer;

	switch(result->type) {
	// This effectively means select_STATEMENT, but we have to assign ID's inside this function
	// and so need to propagate them out
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		HASH128_STATE_INIT(state, 0);
		hash_canonical_query(&state, result);
		routine_len = generate_routine_name(&state, routine_name, MAX_STR_CONST, OutputPlan);
		if (routine_len < 0) {
			FATAL(ERR_PLAN_HASH_FAILED, "");
		}
		GET_FULL_PATH_OF_GENERATED_M_FILE(filename, &routine_name[1]);	/* updates "filename" to be full path */
		if (access(filename, F_OK) == -1) {	// file doesn't exist
			INFO(CUSTOM_ERROR, "Generating M file [%s] (to execute SQL query)", filename);
			filename_lock = make_buffers("^%ydboctoocto", 2, "files", filename);
			// Wait for 5 seconds in case another process is writing to same filename
			ydb_lock_incr_s(5000000000, &filename_lock[0], 2, &filename_lock[1]);
			if (access(filename, F_OK) == -1) {
				pplan = emit_select_statement(result, filename);
				if(pplan == NULL) {
					octo_cfree(memory_chunks);
					ydb_lock_decr_s(&filename_lock[0], 2, &filename_lock[1]);
					result = NULL;
					return 1;
				}
				assert(pplan != NULL);
			}
			ydb_lock_decr_s(&filename_lock[0], 2, &filename_lock[1]);
			free(filename_lock);
		} else {
			INFO(CUSTOM_ERROR, "Using already generated M file [%s] (to execute SQL query)", filename);
		}
		cursorId = atol(cursor_exe_global.buf_addr);
		ci_filename.address = filename;
		ci_filename.length = strlen(filename);
		ci_routine.address = routine_name;
		ci_routine.length = routine_len;
		status = ydb_ci("_ydboctoselect", cursorId, &ci_filename, &ci_routine);
		YDB_ERROR_CHECK(status);
		(*callback)(result, cursorId, parms, filename);
		// Deciding to free the select_STATEMENT must be done by the caller, as they may want to rerun it or send row
		// descriptions hence the decision to not free the memory_chunk below.
		free_memory_chunks = false;
		break;
	case table_STATEMENT:
		buffer = octo_cmalloc(memory_chunks, 5);
		out = open_memstream(&buffer, &buffer_size);
		assert(out);
		emit_create_table(out, result);
		fclose(out);
		INFO(CUSTOM_ERROR, "%s", buffer);

		YDB_MALLOC_BUFFER(table_name_buffer, MAX_STR_CONST);
		YDB_MALLOC_BUFFER(table_sub_buffer, MAX_STR_CONST);
		YDB_MALLOC_BUFFER(&table_create_buffer, MAX_STR_CONST);
		SqlTable *table;
		UNPACK_SQL_STATEMENT(table, result, table);
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		YDB_COPY_STRING_TO_BUFFER(value->v.reference, table_name_buffer, done);
		if(!done) {
			FATAL(ERR_TABLE_DEFINITION_TOO_LONG, value->v.reference,
					table_name_buffer->len_alloc,
					strlen(value->v.reference));
		}
		YDB_COPY_STRING_TO_BUFFER(buffer, &table_create_buffer, done);
		if(!done) {
			FATAL(ERR_TABLE_DEFINITION_TOO_LONG, value->v.reference,
					table_create_buffer.len_alloc,
					strlen(buffer));
		}
		YDB_STRING_TO_BUFFER("t", table_type_buffer);
		status = ydb_set_s(&schema_global, 2,
				   table_name_buffers,
				   &table_create_buffer);
		YDB_ERROR_CHECK(status);
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
			cur_length += MAX_STR_CONST;
			i++;
		}
		YDB_STRING_TO_BUFFER("l", table_type_buffer);
		table_create_buffer.len_used = snprintf(table_create_buffer.buf_addr, MAX_STR_CONST, "%d", length);
		status = ydb_set_s(&schema_global, 2,
				   table_name_buffers,
				   &table_create_buffer);
		store_table_in_pg_class(table);
		free(buffer);
		free(table_buffer);
		YDB_FREE_BUFFER(table_name_buffer);
		YDB_FREE_BUFFER(table_sub_buffer);
		YDB_FREE_BUFFER(&table_create_buffer);
		break;
	case drop_STATEMENT:
		YDB_MALLOC_BUFFER(table_name_buffer, MAX_STR_CONST);
		YDB_COPY_STRING_TO_BUFFER(result->v.drop->table_name->v.value->v.reference, table_name_buffer, done);
		status = ydb_delete_s(&schema_global, 1,
				      table_name_buffer,
				      YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		// If we had the table loaded, drop it from memory
		ydb_buffer_t *table_stmt = get("%ydboctoloadedschemas", 1,
				result->v.drop->table_name->v.value->v.reference);
		if(table_stmt != NULL) {
			buffer = *((char**)table_stmt->buf_addr);
			free(buffer);
			free(table_stmt->buf_addr);
			free(table_stmt);
			YDB_LITERAL_TO_BUFFER("%ydboctoloadedschemas", &schema_global);
			status = ydb_delete_s(&schema_global, 1,
					table_name_buffer,
					YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
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
		FATAL(ERR_FEATURE_NOT_IMPLEMENTED, query);
	}
	YDB_FREE_BUFFER(&cursor_exe_global);
	if (free_memory_chunks) {
		octo_cfree(memory_chunks);
		memory_chunks = NULL;
	}
	result = NULL;
	return 1;
}
