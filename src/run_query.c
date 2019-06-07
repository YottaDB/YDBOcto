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
	SqlTable	*temp_table;
	SqlValue	*value;
	bool		free_memory_chunks;
	char		*buffer, filename[MAX_STR_CONST], routine_name[MAX_ROUTINE_LEN];
	gtm_long_t	cursorId;
	hash128_state_t	state;
	int		done, routine_len = 0;
	int		status;
	size_t		buffer_size = 0;
	ydb_buffer_t	*filename_lock = NULL;
	ydb_buffer_t	z_status, z_status_value;
	ydb_string_t	ci_filename, ci_routine;

	memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);

	INFO(CUSTOM_ERROR, "Parsing SQL command %s", query);
	result = parse_line(query);
	INFO(CUSTOM_ERROR, "Done!");
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
	ydb_buffer_t	schema_global, table_name_buffer, table_create_buffer;

	YDB_MALLOC_BUFFER(&cursor_exe_global, MAX_STR_CONST);
	YDB_MALLOC_BUFFER(&table_name_buffer, MAX_STR_CONST);
	YDB_MALLOC_BUFFER(&table_create_buffer, MAX_STR_CONST);
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	status = ydb_incr_s(&schema_global, 0, NULL, NULL, &cursor_exe_global);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	cursor_exe_global.buf_addr[cursor_exe_global.len_used] = '\0';
	INFO(CUSTOM_ERROR, "Generating SQL for cursor %s", cursor_exe_global.buf_addr);
	free_memory_chunks = true;	// By default run "octo_cfree(memory_chunks)" at the end
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
		snprintf(filename, MAX_STR_CONST, "%s/_%s.m", config->tmp_dir, &routine_name[1]);
		if (access(filename, F_OK) == -1) {	// file doesn't exist
			filename_lock = make_buffers("^%ydboctoocto", 2, "files", filename);
			// Wait for 5 seconds in case another process is writing to same filename
			ydb_lock_incr_s(5000000000, &filename_lock[0], 2, &filename_lock[1]);
			if (access(filename, F_OK) == -1) {
				pplan = emit_select_statement(result, filename);
				assert(pplan != NULL);
			}
			ydb_lock_decr_s(&filename_lock[0], 2, &filename_lock[1]);
			free(filename_lock);
		}
		cursorId = atol(cursor_exe_global.buf_addr);
		ci_filename.address = filename;
		ci_filename.length = strlen(filename);
		ci_routine.address = routine_name;
		ci_routine.length = routine_len;
		SWITCH_FROM_OCTO_GLOBAL_DIRECTORY();
		status = ydb_ci("select", cursorId, &ci_filename, &ci_routine);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		SWITCH_TO_OCTO_GLOBAL_DIRECTORY();
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
		UNPACK_SQL_STATEMENT(value, result->v.table->tableName, value);
		YDB_COPY_STRING_TO_BUFFER(value->v.reference, &table_name_buffer, done);
		if(!done) {
			FATAL(ERR_TABLE_DEFINITION_TOO_LONG, value->v.reference,
					table_name_buffer.len_alloc,
					strlen(value->v.reference));
		}
		YDB_COPY_STRING_TO_BUFFER(buffer, &table_create_buffer, done);
		if(!done) {
			FATAL(ERR_TABLE_DEFINITION_TOO_LONG, value->v.reference,
					table_create_buffer.len_alloc,
					strlen(buffer));
		}
		status = ydb_set_s(&schema_global, 1,
				   &table_name_buffer,
				   &table_create_buffer);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		/// TODO: we should drop preexisting tables from here
		if(definedTables == NULL) {
			definedTables = result->v.table;
			dqinit(definedTables);
		} else {
			dqinsert(definedTables, result->v.table, temp_table);
		}
		free_memory_chunks = false; // Don't free tables which will be used during this process
		// When https://gitlab.com/YottaDB/DBMS/YDBOcto/issues/107 is completed, the above line can be removed
		break;
	case drop_STATEMENT:
		YDB_COPY_STRING_TO_BUFFER(result->v.drop->table_name->v.value->v.reference, &table_name_buffer, done);
		status = ydb_delete_s(&schema_global, 1,
				      &table_name_buffer,
				      YDB_DEL_NODE);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		/// TODO: we should drop tables here
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
	YDB_FREE_BUFFER(&table_name_buffer);
	YDB_FREE_BUFFER(&table_create_buffer);
	YDB_FREE_BUFFER(&cursor_exe_global);
	if (free_memory_chunks)
		octo_cfree(memory_chunks);
	result = NULL;
	return 1;
}
