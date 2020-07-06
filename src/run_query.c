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

#include "mmrhash.h"

#include "octo.h"
#include "octo_types.h"
#include "physical_plan.h"
#include "parser.h"
#include "lexer.h"
#include "helpers.h"

#define TIMEOUT_1_SEC  ((unsigned long long)1000000000)
#define TIMEOUT_10_SEC (10 * TIMEOUT_1_SEC)

#define CLEANUP_AND_RETURN(MEMORY_CHUNKS, BUFFER, TABLE_SUB_BUFFER, TABLE_BUFFER, QUERY_LOCK)                       \
	{                                                                                                           \
		if (NULL != BUFFER) {                                                                               \
			free(BUFFER);                                                                               \
		}                                                                                                   \
		if (NULL != TABLE_SUB_BUFFER) {                                                                     \
			YDB_FREE_BUFFER(TABLE_SUB_BUFFER);                                                          \
		}                                                                                                   \
		if (NULL != TABLE_BUFFER) {                                                                         \
			free(TABLE_BUFFER);                                                                         \
		}                                                                                                   \
		if (NULL != QUERY_LOCK) {                                                                           \
			int lclStatus;                                                                              \
                                                                                                                    \
			lclStatus = ydb_lock_decr_s((ydb_buffer_t *)QUERY_LOCK, 1, (ydb_buffer_t *)QUERY_LOCK + 1); \
			YDB_ERROR_CHECK(lclStatus);                                                                 \
		}                                                                                                   \
		OCTO_CFREE(MEMORY_CHUNKS);                                                                          \
		return 1;                                                                                           \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, MEMORY_CHUNKS, BUFFER, TABLE_SUB_BUFFER, TABLE_BUFFER, QUERY_LOCK) \
	{                                                                                                           \
		YDB_ERROR_CHECK(STATUS);                                                                            \
		if (YDB_OK != STATUS) {                                                                             \
			CLEANUP_AND_RETURN(MEMORY_CHUNKS, BUFFER, TABLE_SUB_BUFFER, TABLE_BUFFER, QUERY_LOCK);      \
		}                                                                                                   \
	}

#define SETUP_PLAN_METADATA_DB_NODE(PLAN_FILENAME, VARNAME, SUBS_ARRAY)       \
	{                                                                     \
		YDB_STRING_TO_BUFFER(config->global_names.octo, &VARNAME);    \
		YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &SUBS_ARRAY[0]); \
		YDB_STRING_TO_BUFFER(PLAN_FILENAME, &SUBS_ARRAY[1]);          \
	}

#define GET_PLAN_METADATA_DB_NODE(PLAN_FILENAME, DB_NODE_FOUND, STATUS)          \
	{                                                                        \
		ydb_buffer_t varname, subs_array[2];                             \
                                                                                 \
		SETUP_PLAN_METADATA_DB_NODE(PLAN_FILENAME, varname, subs_array); \
		STATUS = ydb_data_s(&varname, 2, subs_array, &DB_NODE_FOUND);    \
	}

#define DELETE_PLAN_METADATA_DB_NODE(PLAN_FILENAME, STATUS)                      \
	{                                                                        \
		ydb_buffer_t varname, subs_array[2];                             \
                                                                                 \
		SETUP_PLAN_METADATA_DB_NODE(PLAN_FILENAME, varname, subs_array); \
		STATUS = ydb_delete_s(&varname, 2, subs_array, YDB_DEL_TREE);    \
	}

#define CLEANUP_FILENAME_LOCK(I, FILENAME_LOCK, STATUS)                                                   \
	{                                                                                                 \
		if (1 == I) {                                                                             \
			/* If this is the second iteration, release the lock obtained in first iteration. \
			 * Cannot do much if call fails. Hence no check of return status.                 \
			 */                                                                               \
			STATUS = ydb_lock_decr_s(&FILENAME_LOCK[0], 2, &FILENAME_LOCK[1]);                \
			YDB_ERROR_CHECK(STATUS);                                                          \
		}                                                                                         \
		if (NULL != FILENAME_LOCK) {                                                              \
			free(FILENAME_LOCK);                                                              \
		}                                                                                         \
	}

#define CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(QUERY_LOCK, MEMORY_CHUNKS)         \
	{                                                                       \
		int lclStatus;                                                  \
                                                                                \
		lclStatus = ydb_lock_decr_s(&QUERY_LOCK[0], 2, &QUERY_LOCK[1]); \
		YDB_ERROR_CHECK(lclStatus);                                     \
		OCTO_CFREE(MEMORY_CHUNKS);                                      \
	}

#define CLEANUP_FROM_PLAN_GENERATION(I, FILENAME_LOCK, QUERY_LOCK, MEMORY_CHUNKS)                       \
	{                                                                                               \
		int lclStatus;                                                                          \
                                                                                                        \
		CLEANUP_FILENAME_LOCK(I, FILENAME_LOCK, lclStatus); /* lclStatus is set but not used */ \
		/* No need to use lclStatus to return a non-zero value since the caller of this macro   \
		 * is already in a code path that returns a non-zero value to signify an error.         \
		 */                                                                                     \
		CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(QUERY_LOCK, MEMORY_CHUNKS);                        \
	}

/* Runs a query that has already been read and parsed. Creates a logical and physical plan if necessary. And executes it.
 * Returns
 *	 0 for normal
 *	 1 for error
 *	-1 if query has been canceled.
 */
int run_query(callback_fnptr_t callback, void *parms, boolean_t send_row_description, ParseContext *parse_context) {
	FILE *		out;
	PhysicalPlan *	pplan;
	SqlStatement *	result;
	SqlValue *	value;
	bool		free_memory_chunks;
	char *		buffer, filename[OCTO_PATH_MAX], routine_name[MAX_ROUTINE_LEN];
	char		placeholder;
	ydb_long_t	cursorId;
	hash128_state_t state;
	int		done, routine_len = MAX_ROUTINE_LEN;
	int		status;
	size_t		buffer_size = 0;
	ydb_buffer_t *	filename_lock, query_lock[3];
	ydb_string_t	ci_filename, ci_routine;
	HIST_ENTRY *	cur_hist;
	ydb_buffer_t	cursor_local;
	ydb_buffer_t	cursor_ydb_buff;
	ydb_buffer_t	schema_global;
	ydb_buffer_t	octo_global;
	boolean_t	canceled = FALSE, cursor_used;
	int		length;
	int		cur_length;
	int		i;

	SqlTable *    table;
	char *	      tablename;
	char *	      table_buffer;
	ydb_buffer_t  table_name_buffers[3];
	ydb_buffer_t *table_name_buffer, *table_sub_buffer;
	ydb_buffer_t  table_binary_buffer, table_create_buffer;

	SqlFunction * function;
	char *	      function_name;
	char *	      function_buffer;
	ydb_buffer_t  function_name_buffers[4];
	ydb_buffer_t *function_name_buffer, *function_sub_buffer;
	ydb_buffer_t  function_binary_buffer, function_create_buffer;
	char	      cursor_buffer[INT64_TO_STRING_MAX];
	char	      pid_buffer[INT64_TO_STRING_MAX]; /* assume max pid is 64 bits even though it is a 4-byte quantity */
	boolean_t     release_query_lock;

	table_sub_buffer = NULL;
	memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
	// Assign cursor prior to parsing to allow tracking and storage of literal parameters under the cursor local variable
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	cursor_ydb_buff.buf_addr = cursor_buffer;
	cursor_ydb_buff.len_alloc = sizeof(cursor_buffer);
	cursorId = create_cursor(&schema_global, &cursor_ydb_buff);
	if (0 > cursorId) {
		OCTO_CFREE(memory_chunks);
		return 1;
	}
	parse_context->cursorId = cursorId;
	parse_context->cursorIdString = cursor_ydb_buff.buf_addr;

	/* Hold a shared lock before parsing ANY query.
	 *
	 * 1) Read-only queries (i.e. SELECT *) hold on to this shared lock during the parse phase and execution phase.
	 * 2) DDL changing queries (i.e. CREATE TABLE, DROP TABLE) hold on to this shared lock during the parse phase
	 *    but move on to an exclusive lock during the execution phase (when they make changes to the underlying M globals).
	 *
	 * This is to ensure DDL changes do not happen while we are reading M globals as part of parsing the query.
	 *
	 * 1) is implemented by each read-only query getting a lock on ^%ydboctoocto(OCTOLIT_DDL,<pid>) before parsing the query
	 *    and releasing it after parsing and execution of the query (at the end of "run_query.c").
	 * 2) is implemented by getting a lock on ^%ydboctoocto(OCTOLIT_DDL,<pid>) before parsing the query. Once the parse is done
	 *    and the query is going to be executed, we release this lock and instead get a lock on ^%ydboctoocto(OCTOLIT_DDL)
	 *    which will be obtainable only if all other shared queries are done releasing ^%ydboctoocto(OCTOLIT_DDL,<pid>)
	 *   (i.e. no other read-only query is in the parsing or execution phase).
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &query_lock[0]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_DDL, &query_lock[1]);
	/* We have allocated INT64_TO_STRING_MAX bytes which can store at most an 8-byte quantity hence the 8 in assert below */
	assert((INT64_TO_STRING_MAX == sizeof(pid_buffer)) && (8 >= sizeof(pid_t)));
	query_lock[2].buf_addr = pid_buffer;
	query_lock[2].len_alloc = sizeof(pid_buffer);
	query_lock[2].len_used = snprintf(query_lock[2].buf_addr, query_lock[2].len_alloc, "%lld", (long long)config->process_id);
	assert(query_lock[2].len_used < query_lock[2].len_alloc);
	/* Wait 10 seconds for the shared query lock */
	status = ydb_lock_incr_s(TIMEOUT_10_SEC, &query_lock[0], 2, &query_lock[1]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		OCTO_CFREE(memory_chunks);
		return 1;
	}
	release_query_lock = TRUE;
	/* To print only the current query store the index for the last one
	 * then print the difference between the cur_input_index - old_input_index
	 */
	old_input_index = cur_input_index;
	result = parse_line(parse_context);

	/* add the current query to the readlines history */
	if (config->is_tty) {
		if (EOF_CTRLD == eof_hit) {
			/* If Octo was started without an input file (i.e. sitting at the "OCTO>" prompt) and
			 * Ctrl-D was pressed by the user, then print a newline to cleanly terminate the current line
			 * before exiting. No need to do this in case EXIT or QUIT commands were used as we will not
			 * be sitting at the "OCTO>" prompt in that case.
			 */
			printf("\n");
		}
		placeholder = input_buffer_combined[cur_input_index];
		input_buffer_combined[cur_input_index] = '\0';
		/* get the last item added to the history
		 * if it is the same as the current query don't add it to thhe history again
		 */
		cur_hist = history_get(history_length);
		if (NULL != cur_hist) {
			if (0 != strcmp(cur_hist->line, input_buffer_combined + old_input_index))
				add_history(input_buffer_combined + old_input_index);
		} else {
			add_history(input_buffer_combined + old_input_index);
		}
		input_buffer_combined[cur_input_index] = placeholder;
	}

	INFO(CUSTOM_ERROR, "Parsing done for SQL command [%.*s]", cur_input_index - old_input_index,
	     input_buffer_combined + old_input_index);
	if (NULL == result) {
		INFO(CUSTOM_ERROR, "Returning failure from run_query");
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		YDB_ERROR_CHECK(status);
		OCTO_CFREE(memory_chunks);
		return 1;
	}
	if (config->dry_run || (no_data_STATEMENT == result->type)) {
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		YDB_ERROR_CHECK(status);
		OCTO_CFREE(memory_chunks);
		return (YDB_OK != status);
	}
	INFO(CUSTOM_ERROR, "Generating SQL for cursor %s", cursor_ydb_buff.buf_addr);
	free_memory_chunks = true; // By default run "octo_cfree(memory_chunks)" at the end

	switch (result->type) {
	case set_operation_STATEMENT:
	case table_alias_STATEMENT:
	case show_STATEMENT:
	case set_STATEMENT:
		cursor_used = TRUE;
		break;
	default:
		// Other statement types don't require the cursor, so just free it now
		cursor_used = FALSE;
		break;
	}

	switch (result->type) {
	// This effectively means select_STATEMENT, but we have to assign ID's inside this function
	// and so need to propagate them out
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		TRACE(ERR_ENTERING_FUNCTION, "hash_canonical_query");
		INVOKE_HASH_CANONICAL_QUERY(state, result, status); /* "state" holds final hash */
		if (0 != status) {
			status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
			YDB_ERROR_CHECK(status);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		status = generate_routine_name(&state, routine_name, routine_len, OutputPlan);
		if (1 == status) {
			ERROR(ERR_PLAN_HASH_FAILED, "");
			status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
			YDB_ERROR_CHECK(status);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		/* The below call updates "filename" to be the full path including "routine_name" at the end */
		status = get_full_path_of_generated_m_file(filename, sizeof(filename), &routine_name[1]);
		if (status) {
			ERROR(ERR_PLAN_HASH_FAILED, "");
			status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
			YDB_ERROR_CHECK(status);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		filename_lock = NULL; /* used by CLEANUP_FROM_PLAN_GENERATION macro to know whether to invoke free() or not */
		for (i = 0; i < 2; i++) {
			/* i = 0 is the iteration BEFORE we get the M lock (to generate the plan).
			 * i = 1 is the iteration AFTER  we get the M lock (to generate the plan).
			 *
			 * The code to do checks is mostly common for both iterations hence this for loop to avoid code duplication.
			 */
			boolean_t generate_plan;

			generate_plan = (-1 == access(filename, F_OK));
			if (!generate_plan) {
				/* The plan exists (i.e. has already been generated). But check if the corresponding nodes
				 * in the database are in sync as well. If not, regenerate the plan. This way we will avoid
				 * an ERR_DATABASE_FILES_OOS error later.
				 */
				unsigned int db_node_found;

				GET_PLAN_METADATA_DB_NODE(filename, db_node_found, status); /* sets "db_node_found" and "status" */
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					CLEANUP_FROM_PLAN_GENERATION(i, filename_lock, query_lock, memory_chunks);
					return 1;
				}
				if (0 == db_node_found) {
					/* Plan exists but no corresponding db node was found. Regenerate plan. */
					generate_plan = TRUE;
				}
			}
			if (generate_plan) {
				if (0 == i) {
					/* Get the M lock and redo the check of whether the plan is still not generated */
					filename_lock = make_buffers(config->global_names.octo, 2, OCTOLIT_FILES, filename);
					/* Wait for 5 seconds in case another process is writing to same filename */
					status = ydb_lock_incr_s(5 * TIMEOUT_1_SEC, &filename_lock[0], 2, &filename_lock[1]);
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						CLEANUP_FROM_PLAN_GENERATION(i, filename_lock, query_lock, memory_chunks);
						return 1;
					}
					continue; /* So we redo the check of whether plan exists or not after getting lock */
				} else {
					/* We got the lock and the plan still does not exist. Generate the plan this time around. */
					INFO(CUSTOM_ERROR, "Generating M file [%s] (to execute SQL query)", filename);
					pplan = emit_select_statement(result, filename);
					if (NULL == pplan) {
						CLEANUP_FROM_PLAN_GENERATION(i, filename_lock, query_lock, memory_chunks);
						return 1;
					}
				}
			} else {
				/* Plan was found to already exist. So reuse it. */
				INFO(CUSTOM_ERROR, "Using already generated M file [%s] (to execute SQL query)", filename);
				/* If this is the first iteration, then we can break out of the loop but if the second
				 * iteration, then we need to release the locks obtained in the first iteration.
				 */
				if (0 == i) {
					break;
				}
			}
			assert(1 == i);
			CLEANUP_FILENAME_LOCK(i, filename_lock, status);
			if (YDB_OK != status) {
				CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks);
				return 1;
			}
		}
		if (parse_context->is_extended_query) {
			memcpy(parse_context->routine, routine_name, routine_len);
			status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
			YDB_ERROR_CHECK(status);
			OCTO_CFREE(memory_chunks);
			return (YDB_OK != status);
		}
		cursorId = atol(cursor_ydb_buff.buf_addr);
		ci_filename.address = filename;
		ci_filename.length = strlen(filename);
		ci_routine.address = routine_name;
		ci_routine.length = routine_len;
		// Call the select routine
		// cursorId is typecast here since the YottaDB call-in interface does not yet support 64-bit parameters
		status = ydb_ci("_ydboctoselect", cursorId, &ci_filename, &ci_routine);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
			YDB_ERROR_CHECK(status);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		// Check for cancel requests only if running rocto
		if (config->is_rocto) {
			canceled = is_query_canceled(callback, cursorId, parms, filename, send_row_description);
			if (canceled) {
				status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
				YDB_ERROR_CHECK(status);
				OCTO_CFREE(memory_chunks);
				return -1;
			}
		}
		assert(!config->is_rocto || (NULL != parms));
		status = (*callback)(result, cursorId, parms, filename, send_row_description);
		if (0 != status) {
			status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
			YDB_ERROR_CHECK(status);
			// May be freed in the callback function, must check before freeing
			if (NULL != memory_chunks) {
				OCTO_CFREE(memory_chunks);
			}
			return 1;
		}
		// Deciding to free the select_STATEMENT must be done by the caller, as they may want to rerun it or send
		// row descriptions hence the decision to not free the memory_chunk below.
		free_memory_chunks = false;
		break;
	case drop_table_STATEMENT:   /* DROP TABLE */
	case create_table_STATEMENT: /* CREATE TABLE */
		/* Note that CREATE/DROP TABLE is very similar to CREATE/DROP FUNCTION, and changes to either may need to be
		 * reflected in the other.
		 *
		 * A CREATE TABLE should do a DROP TABLE followed by a CREATE TABLE hence merging the two cases above
		 */
		table_name_buffer = &table_name_buffers[0];
		/* Initialize a few variables to NULL at the start. They are really used much later but any calls to
		 * CLEANUP_AND_RETURN and CLEANUP_AND_RETURN_IF_NOT_YDB_OK before then need this so they skip freeing this.
		 */
		buffer = NULL;
		table_sub_buffer = NULL;
		table_buffer = NULL;
		/* Now release the shared query lock and get an exclusive query lock to do DDL changes */
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, table_sub_buffer, table_buffer, NULL);
		/* Wait 10 seconds for the exclusive DDL change lock */
		status = ydb_lock_incr_s(TIMEOUT_10_SEC, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, table_sub_buffer, table_buffer, NULL);
		/* Note: Last parameter is NULL above as we do not have any query lock to release
		 * at this point since query lock grab failed.
		 */
		/* First get a ydb_buffer_t of the table name into "table_name_buffer" */
		if (drop_table_STATEMENT == result->type) {
			tablename = result->v.drop_table->table_name->v.value->v.reference;
			table = NULL;
		} else {
			UNPACK_SQL_STATEMENT(table, result, create_table);
			UNPACK_SQL_STATEMENT(value, table->tableName, value);
			tablename = value->v.reference;
		}
		YDB_STRING_TO_BUFFER(tablename, table_name_buffer);
		/* Check if OIDs were created for this table.
		 * If so, delete those nodes from the catalog now that this table is going away.
		 */
		status = delete_table_from_pg_class(table_name_buffer);
		if (0 != status) {
			CLEANUP_AND_RETURN(memory_chunks, buffer, table_sub_buffer, table_buffer, query_lock);
		}
		/* Now that OIDs have been cleaned up, dropping the table is a simple : KILL ^%ydboctoschema(TABLENAME) */
		status = ydb_delete_s(&schema_global, 1, table_name_buffer, YDB_DEL_TREE);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, table_sub_buffer, table_buffer, query_lock);
		/* Drop the table from the local cache */
		status = drop_schema_from_local_cache(table_name_buffer, TableSchema);
		if (YDB_OK != status) {
			/* YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()" */
			CLEANUP_AND_RETURN(memory_chunks, buffer, table_sub_buffer, table_buffer, query_lock);
		}
		if (create_table_STATEMENT == result->type) {
			/* CREATE TABLE case. More processing needed. */
			out = open_memstream(&buffer, &buffer_size);
			assert(out);
			status = emit_create_table(out, result);
			fclose(out); // at this point "buffer" and "buffer_size" are usable
			if (0 != status) {
				// Error messages for the non-zero status would already have been issued in
				// "emit_create_table"
				CLEANUP_AND_RETURN(memory_chunks, buffer, table_sub_buffer, table_buffer, query_lock);
			}
			INFO(CUSTOM_ERROR, "%s", buffer); /* print the converted text representation of the CREATE TABLE command */

			YDB_STRING_TO_BUFFER(buffer, &table_create_buffer);
			YDB_STRING_TO_BUFFER("t", &table_name_buffers[1]);
			status = ydb_set_s(&schema_global, 2, table_name_buffers, &table_create_buffer);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, table_sub_buffer, table_buffer, query_lock);
			free(buffer);
			/* Note: "table_create_buffer" (whose "buf_addr" points to "buffer") is also no longer usable */
			buffer = NULL; /* So CLEANUP_AND_RETURN* macro calls below do not try "free(buffer)" */
			/* First store table name in catalog. As we need that OID to store in the binary table definition.
			 * The below call also sets table->oid which is needed before the call to "compress_statement" as
			 * that way the oid also gets stored in the binary table definition.
			 */
			status = store_table_in_pg_class(table, table_name_buffer);
			if (YDB_OK != status) {
				CLEANUP_AND_RETURN(memory_chunks, buffer, table_sub_buffer, table_buffer, query_lock);
			}
			compress_statement(result, &table_buffer, &length);
			assert(NULL != table_buffer);
			table_binary_buffer.len_alloc = MAX_STR_CONST;
			YDB_STRING_TO_BUFFER(OCTOLIT_BINARY, &table_name_buffers[1]);
			table_sub_buffer = &table_name_buffers[2];
			YDB_MALLOC_BUFFER(table_sub_buffer, MAX_STR_CONST);
			i = 0;
			cur_length = 0;
			while (cur_length < length) {
				table_sub_buffer->len_used
				    = snprintf(table_sub_buffer->buf_addr, table_sub_buffer->len_alloc, "%d", i);
				table_binary_buffer.buf_addr = &table_buffer[cur_length];
				if (MAX_STR_CONST < (length - cur_length)) {
					table_binary_buffer.len_used = MAX_STR_CONST;
				} else {
					table_binary_buffer.len_used = length - cur_length;
				}
				status = ydb_set_s(&schema_global, 3, table_name_buffers, &table_binary_buffer);
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, table_sub_buffer, table_buffer,
								 query_lock);
				cur_length += MAX_STR_CONST;
				i++;
			}
			YDB_STRING_TO_BUFFER(OCTOLIT_LENGTH, &table_name_buffers[1]);
			// Use table_sub_buffer as a temporary buffer below
			table_sub_buffer->len_used
			    = snprintf(table_sub_buffer->buf_addr, table_sub_buffer->len_alloc, "%d", length);
			status = ydb_set_s(&schema_global, 2, table_name_buffers, table_sub_buffer);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, table_sub_buffer, table_buffer, query_lock);
			free(table_buffer);
			YDB_FREE_BUFFER(table_sub_buffer);
		}
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset few variables to NULL as those parts of the
			 * cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			table_sub_buffer = NULL;
			table_buffer = NULL;
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, table_sub_buffer, table_buffer, NULL);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
		break;			    /* OCTO_CFREE(memory_chunks) will be done after the "break" */
	case drop_function_STATEMENT:	    /* DROP FUNCTION */
	case create_function_STATEMENT:	    /* CREATE FUNCTION */
		/* Note that CREATE/DROP FUNCTION is very similar to CREATE/DROP TABLE, and changes to either may need to be
		 * reflected in the other.
		 *
		 * A CREATE FUNCTION should do a DROP FUNCTION followed by a CREATE FUNCTION hence merging the two cases
		 * above
		 */
		function_name_buffer = &function_name_buffers[1];
		/* Initialize a few variables to NULL at the start. They are really used much later but any calls to
		 * CLEANUP_AND_RETURN and CLEANUP_AND_RETURN_IF_NOT_YDB_OK before then need this so they skip freeing this.
		 */
		buffer = NULL;
		function_sub_buffer = NULL;
		function_buffer = NULL;
		// Now release the shared query lock and get an exclusive query lock to do DDL changes
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer, NULL);
		// Wait 10 seconds for the exclusive DDL change lock
		status = ydb_lock_incr_s(TIMEOUT_10_SEC, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer, NULL);
		/* Note: Last parameter is NULL above as we do not have any query lock to release
		 * at this point since query lock grab failed.
		 */
		// First get a ydb_buffer_t of the function name into "function_name_buffer"
		if (drop_function_STATEMENT == result->type) {
			function_name = result->v.drop_function->function_name->v.value->v.reference;
			function = NULL;
		} else {
			UNPACK_SQL_STATEMENT(function, result, create_function);
			UNPACK_SQL_STATEMENT(value, function->function_name, value);
			function_name = value->v.reference;
		}
		YDB_STRING_TO_BUFFER(function_name, function_name_buffer);

		// Delete the function from the catalog
		status = delete_function_from_pg_proc(function_name_buffer);
		if (0 != status) {
			CLEANUP_AND_RETURN(memory_chunks, buffer, function_sub_buffer, function_buffer, query_lock);
		}
		/* Ensure all _ydboctoP*.m plans that rely on this function are recreated by deleting those database nodes
		 * that correspond to the plan metadata of these plans. We do not delete the _ydboctoP*.m files since
		 * the _ydboctoP*.o files would anyways exist and also need to be removed but it is not straightforward (since
		 * we need to find the first obj directory in the zroutines list). Just deleting the database nodes is enough
		 * since that is checked every time using the GET_PLAN_METADATA_DB_NODE macro before using a pre-existing plan.
		 */
		YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
		YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_name_buffers[0]);
		YDB_STRING_TO_BUFFER(OCTOLIT_PLAN_METADATA, &function_name_buffers[2]);
		function_name_buffers[3].buf_addr = filename;
		function_name_buffers[3].len_used = 0;
		function_name_buffers[3].len_alloc = sizeof(filename);
		while (TRUE) {
			status = ydb_subscript_next_s(&octo_global, 4, &function_name_buffers[0], &function_name_buffers[3]);
			if (YDB_ERR_NODEEND == status) {
				break;
			}
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer,
							 query_lock);
			DELETE_PLAN_METADATA_DB_NODE(filename, status);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer,
							 query_lock);
		}
		// Drop the function: KILL ^%ydboctoocto(OCTOLIT_FUNCTIONS,FUNCTIONNAME)
		status = ydb_delete_s(&octo_global, 2, function_name_buffers, YDB_DEL_TREE);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer, query_lock);

		// Drop the function from the local cache
		status = drop_schema_from_local_cache(function_name_buffer, FunctionSchema);
		if (YDB_OK != status) {
			// YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()"
			CLEANUP_AND_RETURN(memory_chunks, buffer, function_sub_buffer, function_buffer, query_lock);
		}

		if (create_function_STATEMENT == result->type) {
			// CREATE FUNCTION case. More processing needed.
			out = open_memstream(&buffer, &buffer_size);
			assert(out);
			status = emit_create_function(out, result);
			fclose(out); // at this point "buffer" and "buffer_size" are usable
			if (0 != status) {
				/* Error messages for the non-zero status would already have been issued in
				 * "emit_create_function"
				 */
				CLEANUP_AND_RETURN(memory_chunks, buffer, function_sub_buffer, function_buffer, query_lock);
			}
			INFO(CUSTOM_ERROR, "%s", buffer); /* print the converted text representation of the CREATE TABLE command */

			YDB_STRING_TO_BUFFER(buffer, &function_create_buffer);
			YDB_STRING_TO_BUFFER("t", &function_name_buffers[2]); // 't' for "text" representation
			/* Store the text representation of the CREATE FUNCTION statement:
			 *	^ydboctoocto(OCTOLIT_FUNCTIONS,function_name,"t")
			 */
			status = ydb_set_s(&octo_global, 3, function_name_buffers, &function_create_buffer);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer,
							 query_lock);
			free(buffer);
			/* Note: "function_create_buffer" (whose "buf_addr" points to "buffer") is also no longer unusable */
			buffer = NULL; // So CLEANUP_AND_RETURN* macro calls below do not try "free(buffer)"

			/* Store the extrinsic function label from the CREATE FUNCTION statement:
			 *	^ydboctoocto(OCTOLIT_FUNCTIONS,function_name)=$EXTRINSIC_FUNCTION
			 */
			YDB_STRING_TO_BUFFER(function->extrinsic_function->v.value->v.string_literal, &function_name_buffers[2]);
			status = ydb_set_s(&octo_global, 2, &function_name_buffers[0], &function_name_buffers[2]);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer,
							 query_lock);

			/* First store function name in catalog. As we need that OID to store in the binary table
			 * definition. The below call also sets table->oid which is needed before the call to
			 * "compress_statement" as that way the oid also gets stored in the binary table definition.
			 */
			status = store_function_in_pg_proc(function);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer,
							 query_lock);

			compress_statement(result, &function_buffer, &length);
			assert(NULL != function_buffer);
			function_binary_buffer.len_alloc = MAX_STR_CONST;
			YDB_STRING_TO_BUFFER(OCTOLIT_BINARY, &function_name_buffers[2]);
			function_sub_buffer = &function_name_buffers[3];
			YDB_MALLOC_BUFFER(function_sub_buffer, MAX_STR_CONST);
			i = 0;
			cur_length = 0;
			while (cur_length < length) {
				function_sub_buffer->len_used
				    = snprintf(function_sub_buffer->buf_addr, function_sub_buffer->len_alloc, "%d", i);
				function_binary_buffer.buf_addr = &function_buffer[cur_length];
				if (MAX_STR_CONST < (length - cur_length)) {
					function_binary_buffer.len_used = MAX_STR_CONST;
				} else {
					function_binary_buffer.len_used = length - cur_length;
				}
				status = ydb_set_s(&octo_global, 4, function_name_buffers, &function_binary_buffer);
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer,
								 function_buffer, query_lock);
				cur_length += MAX_STR_CONST;
				i++;
			}
			YDB_STRING_TO_BUFFER(OCTOLIT_LENGTH, &function_name_buffers[2]);
			// Use function_sub_buffer as a temporary buffer below
			function_sub_buffer->len_used
			    = snprintf(function_sub_buffer->buf_addr, function_sub_buffer->len_alloc, "%d", length);
			status = ydb_set_s(&octo_global, 3, function_name_buffers, function_sub_buffer);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer,
							 query_lock);
			free(function_buffer);
			YDB_FREE_BUFFER(function_sub_buffer);
		}
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset few variables to NULL as those parts of the
			 * cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			function_sub_buffer = NULL;
			function_buffer = NULL;
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, function_sub_buffer, function_buffer, NULL);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
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
		cursorId = atol(cursor_ydb_buff.buf_addr);
		(*callback)(result, cursorId, parms, NULL, send_row_description);
		break;
	case index_STATEMENT:
		break;
	default:
		WARNING(ERR_FEATURE_NOT_IMPLEMENTED, input_buffer_combined);
		break;
	}
	// Must free the cursor buffer now if it was used for a statement type that required it
	if (cursor_used) {
		// Cleanup cursor
		if (!parse_context->skip_cursor_cleanup) {
			YDB_MALLOC_BUFFER(&cursor_local, INT64_TO_STRING_MAX);
			YDB_COPY_STRING_TO_BUFFER("%ydboctocursor", &cursor_local, done);
			if (done) {
				status = ydb_delete_s(&cursor_local, 1, &cursor_ydb_buff, YDB_DEL_TREE);
				YDB_ERROR_CHECK(status);
			} else {
				ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
			}
			YDB_FREE_BUFFER(&cursor_local);
		}
	}
	if (release_query_lock) {
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		if (YDB_OK != status) {
			YDB_ERROR_CHECK(status);
		}
	}
	if (free_memory_chunks) {
		OCTO_CFREE(memory_chunks);
	}
	return 0;
}
