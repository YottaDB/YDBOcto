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

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "mmrhash.h"

#include "octo.h"
#include "octo_types.h"
#include "physical_plan.h"
#include "parser.h"
#include "lexer.h"
#include "helpers.h"

#define CLEANUP_AND_RETURN(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK)                                         \
	{                                                                                                           \
		if (NULL != BUFFER) {                                                                               \
			free(BUFFER);                                                                               \
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

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK) \
	{                                                                                         \
		YDB_ERROR_CHECK(STATUS);                                                          \
		if (YDB_OK != STATUS) {                                                           \
			CLEANUP_AND_RETURN(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK);      \
		}                                                                                 \
	}

#define SETUP_PLAN_METADATA_DB_NODE(PLAN_FILENAME, VARNAME, SUBS_ARRAY)       \
	{                                                                     \
		YDB_STRING_TO_BUFFER(config->global_names.octo, &VARNAME);    \
		YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &SUBS_ARRAY[0]); \
		SUBS_ARRAY[1] = PLAN_FILENAME;                                \
	}

#define GET_PLAN_METADATA_DB_NODE(PLAN_FILENAME, DB_NODE_FOUND, STATUS)          \
	{                                                                        \
		ydb_buffer_t varname, subs_array[2];                             \
                                                                                 \
		SETUP_PLAN_METADATA_DB_NODE(PLAN_FILENAME, varname, subs_array); \
		STATUS = ydb_data_s(&varname, 2, subs_array, &DB_NODE_FOUND);    \
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
	char *		buffer, filename[OCTO_PATH_MAX], routine_name[MAX_ROUTINE_LEN], function_hash[MAX_ROUTINE_LEN];
	ydb_long_t	cursorId;
	hash128_state_t state;
	int		done, routine_len = MAX_ROUTINE_LEN, function_hash_len = MAX_ROUTINE_LEN;
	int		status;
	size_t		buffer_size = 0;
	ydb_buffer_t *	filename_lock, query_lock[3], *null_query_lock;
	ydb_string_t	ci_param1, ci_param2;
	ydb_buffer_t	cursor_local;
	ydb_buffer_t	cursor_ydb_buff;
	ydb_buffer_t	schema_global;
	ydb_buffer_t	octo_global;
	boolean_t	canceled = FALSE, cursor_used;
	int		length;
	int		i;
	unsigned int	ret_value;
	SqlTable *	table;
	char *		tablename;
	char *		spcfc_buffer; /* specific buffer (i.e. function-specific or table-specific etc.) */
	ydb_buffer_t	table_name_buffers[3];
	ydb_buffer_t *	table_name_buffer;
	SqlFunction *	function;
	char *		function_name;
	ydb_buffer_t	function_name_buffers[5];
	ydb_buffer_t *	function_name_buffer, *function_hash_buffer;
	char		cursor_buffer[INT64_TO_STRING_MAX];
	char		pid_buffer[INT64_TO_STRING_MAX]; /* assume max pid is 64 bits even though it is a 4-byte quantity */
	boolean_t	release_query_lock;

	// Assign cursor prior to parsing to allow tracking and storage of literal parameters under the cursor local variable
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	cursor_ydb_buff.buf_addr = cursor_buffer;
	cursor_ydb_buff.len_alloc = sizeof(cursor_buffer);
	cursorId = create_cursor(&schema_global, &cursor_ydb_buff);
	if (0 > cursorId) {
		return 1;
	}
	parse_context->cursorId = cursorId;
	parse_context->cursorIdString = cursor_ydb_buff.buf_addr;
	STRCPY_LIT(parse_context->routine, "none", MAX_ROUTINE_LEN);

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
	status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &query_lock[0], 2, &query_lock[1]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	/* To print only the current query store the index for the last one
	 * then print the difference between the cur_input_index - old_input_index
	 */
	old_input_index = cur_input_index;
	memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE); /* needed by "parse_line()" call below */
	result = parse_line(parse_context);
	INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
	if (NULL == result) {
		INFO(INFO_RETURNING_FAILURE, "run_query");
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
	INFO(INFO_CURSOR, cursor_ydb_buff.buf_addr);
	free_memory_chunks = true; // By default run "octo_cfree(memory_chunks)" at the end

	cursor_used = TRUE; /* By default, assume a cursor was used to execute the query */
	release_query_lock = TRUE;
	switch (result->type) {
	// This effectively means select_STATEMENT, but we have to assign ID's inside this function
	// and so need to propagate them out
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
	case insert_STATEMENT:
		TRACE(INFO_ENTERING_FUNCTION, "hash_canonical_query");
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
				ydb_buffer_t filename_buffer;

				YDB_STRING_TO_BUFFER(filename, &filename_buffer);
				GET_PLAN_METADATA_DB_NODE(filename_buffer, db_node_found,
							  status); /* sets "db_node_found" and "status" */
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
					status = ydb_lock_incr_s(TIMEOUT_5_SEC, &filename_lock[0], 2, &filename_lock[1]);
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						CLEANUP_FROM_PLAN_GENERATION(i, filename_lock, query_lock, memory_chunks);
						return 1;
					}
					continue; /* So we redo the check of whether plan exists or not after getting lock */
				} else {
					/* We got the lock and the plan still does not exist. Generate the plan this time around. */
					INFO(INFO_M_PLAN, filename);
					pplan = emit_select_statement(result, filename);
					if (NULL == pplan) {
						CLEANUP_FROM_PLAN_GENERATION(i, filename_lock, query_lock, memory_chunks);
						return 1;
					}
				}
			} else {
				/* Plan was found to already exist. So reuse it. */
				INFO(INFO_REUSE_M_PLAN, filename);
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
		ci_param1.address = filename;
		ci_param1.length = strlen(filename);
		ci_param2.address = routine_name;
		ci_param2.length = routine_len;
		// Call the select routine
		status = ydb_ci("_ydboctoselect", cursorId, &ci_param1, &ci_param2);
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
		/* If this is an INSERT INTO statement type, then there are no result rows to send. So skip
		 * invoking the callback function (to print/send the result rows in case of octo/rocto).
		 * TODO : YDBOcto#502 : We might still need to print something (e.g. 1 row inserted etc.)
		 *	and so some changes might be needed here hence the todo reference.
		 */
		if (insert_STATEMENT != result->type) {
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
		}
		// Deciding to free the select_STATEMENT must be done by the caller, as they may want to rerun it or send
		// row descriptions hence the decision to not free the memory_chunk below.
		free_memory_chunks = false;
		break;
	case discard_all_STATEMENT: /* DISCARD ALL */
		/* Initialize a few variables to NULL at the start. They are used in the CLEANUP_AND_RETURN and
		 * CLEANUP_AND_RETURN_IF_NOT_YDB_OK macro as parameters (we cannot use NULL literal there due to compile errors).
		 */
		buffer = NULL;
		spcfc_buffer = NULL;
		null_query_lock = NULL;
		/* Now release the shared query lock and get an exclusive query lock to do changes to plans/xrefs/triggers */
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
		/* Wait 10 seconds for the exclusive DDL change lock */
		status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
		/* Call an M routine to discard all plans, xrefs and triggers associated with all tables in Octo.
		 * Cannot use SimpleAPI for at least one step (deleting the triggers). Hence using M for all the steps.
		 */
		status = ydb_ci("_ydboctoDiscardAll");
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset a few variables to NULL as those
			 * parts of the cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			assert(NULL == spcfc_buffer);
			assert(NULL == null_query_lock);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
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
		spcfc_buffer = NULL;
		null_query_lock = NULL;
		/* Now release the shared query lock and get an exclusive query lock to do DDL changes */
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
		/* Wait 10 seconds for the exclusive DDL change lock */
		status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
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
			CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
		}
		/* Call an M routine to discard all plans, xrefs and triggers associated with the table being created/dropped.
		 * Cannot use SimpleAPI for at least one step (deleting the triggers). Hence using M for all the steps.
		 */
		ci_param1.address = table_name_buffer->buf_addr;
		ci_param1.length = table_name_buffer->len_used;
		status = ydb_ci("_ydboctoDiscardTable", &ci_param1);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);
		/* Now that OIDs and plan nodes have been cleaned up, dropping the table is a simple
		 *	KILL ^%ydboctoschema(TABLENAME)
		 */
		status = ydb_delete_s(&schema_global, 1, table_name_buffer, YDB_DEL_TREE);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);
		/* Drop the table from the local cache */
		status = drop_schema_from_local_cache(table_name_buffer, TableSchema, NULL);
		if (YDB_OK != status) {
			/* YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()" */
			CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
		}
		if (create_table_STATEMENT == result->type) {
			int text_table_defn_length;

			/* CREATE TABLE case. More processing needed. */
			out = open_memstream(&buffer, &buffer_size);
			assert(out);
			text_table_defn_length = emit_create_table(out, result);
			fclose(out); // at this point "buffer" and "buffer_size" are usable
			if (0 > text_table_defn_length) {
				// Error messages for the negative status would already have been issued in
				// "emit_create_table"
				CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
			}
			INFO(INFO_TEXT_REPRESENTATION,
			     buffer); /* print the converted text representation of the CREATE TABLE command */

			/* Store the text representation of the CREATE TABLE statement:
			 *	^%ydboctoschema(table_name,OCTOLIT_TEXT)
			 */
			status = store_table_definition(table_name_buffers, buffer, text_table_defn_length, TRUE);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);
			free(buffer);
			buffer = NULL; /* So CLEANUP_AND_RETURN* macro calls below do not try "free(buffer)" */
			/* First store table name in catalog. As we need that OID to store in the binary table definition.
			 * The below call also sets table->oid which is needed before the call to "compress_statement" as
			 * that way the oid also gets stored in the binary table definition.
			 */
			status = store_table_in_pg_class(table, table_name_buffer);
			/* Cannot use CLEANUP_AND_RETURN_IF_NOT_YDB_OK macro here because the above function could set
			 * status to 1 to indicate an error (not necessarily a valid YDB_ERR_* code). In case it is a
			 * YDB error code, the YDB_ERROR_CHECK call would have already been done inside "store_table_in_pg_class"
			 * so all we need to do here is check if status is not 0 (aka YDB_OK) and if so invoke CLEANUP_AND_RETURN.
			 */
			if (YDB_OK != status) {
				CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
			}
			compress_statement(result, &spcfc_buffer, &length); /* Sets "spcfc_buffer" to "malloc"ed storage */
			assert(NULL != spcfc_buffer);
			status = store_table_definition(table_name_buffers, spcfc_buffer, length, FALSE);
			free(spcfc_buffer);  /* free buffer that was "malloc"ed in "compress_statement" */
			spcfc_buffer = NULL; /* Now that we did a "free", reset it to NULL */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);
		}
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset few variables to NULL as those parts of the
			 * cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			spcfc_buffer = NULL;
			assert(NULL == null_query_lock);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
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
		spcfc_buffer = NULL;
		null_query_lock = NULL;
		// Now release the shared query lock and get an exclusive query lock to do DDL changes
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
		// Wait 10 seconds for the exclusive DDL change lock
		status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
		/* Note: Last parameter is NULL above as we do not have any query lock to release
		 * at this point since query lock grab failed.
		 */

		// First, get a ydb_buffer_t of the function name into "function_name_buffer"
		if (create_function_STATEMENT == result->type) {
			UNPACK_SQL_STATEMENT(function, result, create_function);
			UNPACK_SQL_STATEMENT(value, function->function_name, value);
			function_name = value->v.reference;
		} else {
			function = NULL;
			function_name = result->v.drop_function->function_name->v.value->v.reference;
		}
		// Then hash the function parameters to determine which function definition is to be CREATEd or DROPed
		INVOKE_HASH_CANONICAL_QUERY(state, result, status); /* "state" holds final hash */
		if (0 != status) {
			CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
		}
		status = generate_routine_name(&state, function_hash, function_hash_len, FunctionHash);
		if (1 == status) {
			CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
		}
		function_hash_buffer = &function_name_buffers[2];
		YDB_STRING_TO_BUFFER(function_hash, function_hash_buffer);
		// Add function hash to parse tree for later addition to logical plan
		if ((NULL != function) && (create_function_STATEMENT == result->type)) {
			SQL_STATEMENT(function->function_hash, value_STATEMENT);
			MALLOC_STATEMENT(function->function_hash, value, SqlValue);
			UNPACK_SQL_STATEMENT(value, function->function_hash, value);
			value->v.string_literal = octo_cmalloc(memory_chunks, function_hash_buffer->len_used + 1);
			memcpy(value->v.string_literal, function_hash, function_hash_buffer->len_used);
			value->v.string_literal[function_hash_buffer->len_used] = '\0'; /* null terminate */
			value->type = FUNCTION_HASH;
		}
		YDB_STRING_TO_BUFFER(function_name, function_name_buffer);

		// Initialize buffers for accessing relevant function GVNs
		YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
		YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_name_buffers[0]);
		function_name_buffers[4].buf_addr = filename;
		function_name_buffers[4].len_used = 0;
		function_name_buffers[4].len_alloc = sizeof(filename);

		if (drop_function_STATEMENT == result->type) {
			status = ydb_data_s(&octo_global, 3, &function_name_buffers[0], &ret_value);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);
			if (0 == ret_value) {
				ERROR(ERR_CANNOT_DROP_FUNCTION, function_name);
				CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
			}
		}
		/* Always drop the function in question, if it exists, either because explicitly requested via DROP or implicitly
		 * when CREATEing or redefining a function.
		 */
		status = delete_function_from_pg_proc(function_name_buffer, function_hash_buffer);
		if (0 != status) {
			CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
		}
		/* Call an M routine to discard all plans associated with the function being created/dropped */
		ci_param1.address = function_name_buffer->buf_addr;
		ci_param1.length = function_name_buffer->len_used;
		ci_param2.address = function_hash_buffer->buf_addr;
		ci_param2.length = function_hash_buffer->len_used;
		status = ydb_ci("_ydboctoDiscardFunction", &ci_param1, &ci_param2);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);

		// Drop the function from the local cache
		status = drop_schema_from_local_cache(function_name_buffer, FunctionSchema, function_hash_buffer);
		if (YDB_OK != status) {
			// YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()"
			CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
		}

		if ((NULL != function) && (create_function_STATEMENT == result->type)) {
			int text_function_defn_length;

			// CREATE FUNCTION case. More processing needed.
			out = open_memstream(&buffer, &buffer_size);
			assert(out);
			text_function_defn_length = emit_create_function(out, result);
			fclose(out); // at this point "buffer" and "buffer_size" are usable
			if (0 > text_function_defn_length) {
				/* Error messages for the non-zero status would already have been issued in
				 * "emit_create_function"
				 */
				CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
			}
			/* Print the converted text representation of the CREATE TABLE command */
			INFO(INFO_TEXT_REPRESENTATION, buffer);
			/* Store the text representation of the CREATE FUNCTION statement:
			 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT)
			 */
			status = store_function_definition(function_name_buffers, buffer, text_function_defn_length, TRUE);
			free(buffer);
			buffer = NULL; // So CLEANUP_AND_RETURN* macro calls below do not try "free(buffer)"

			/* First store function name in catalog. As we need that OID to store in the binary table
			 * definition. The below call also sets table->oid which is needed before the call to
			 * "compress_statement" as that way the oid also gets stored in the binary table definition.
			 * It also checks if there are too many parameters and if so issues an error. Therefore it is best
			 * that we do this step first.
			 */
			status = store_function_in_pg_proc(function, function_hash);
			/* Cannot use CLEANUP_AND_RETURN_IF_NOT_YDB_OK macro here because the above function could set
			 * status to 1 to indicate an error (not necessarily a valid YDB_ERR_* code). In case it is a
			 * YDB error code, the YDB_ERROR_CHECK call would have already been done inside "store_function_in_pg_proc"
			 * so all we need to do here is check if status is not 0 (aka YDB_OK) and if so invoke CLEANUP_AND_RETURN.
			 */
			if (YDB_OK != status) {
				CLEANUP_AND_RETURN(memory_chunks, buffer, spcfc_buffer, query_lock);
			}

			/* Now that we know there are no too-many-parameter errors in ths function, we can safely go ahead
			 * with setting the function related gvn in the database.
			 */
			compress_statement(result, &spcfc_buffer, &length); /* Sets "spcfc_buffer" to "malloc"ed storage */
			assert(NULL != spcfc_buffer);
			status = store_function_definition(function_name_buffers, spcfc_buffer, length, FALSE);
			free(spcfc_buffer);  /* free buffer that was "malloc"ed in "compress_statement" */
			spcfc_buffer = NULL; /* Now that we did a "free", reset it to NULL */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock);
		}
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset few variables to NULL as those parts of the
			 * cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			spcfc_buffer = NULL;
			assert(NULL == null_query_lock);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
		break;
	case begin_STATEMENT:
	case commit_STATEMENT:
		ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "transactions");
		cursor_used = FALSE; /* Remove this line once this feature gets implemented */
		break;
	case set_STATEMENT:
	case show_STATEMENT:
		cursorId = atol(cursor_ydb_buff.buf_addr);
		(*callback)(result, cursorId, parms, NULL, send_row_description);
		break;
	case index_STATEMENT:
		cursor_used = FALSE; /* Remove this line once this feature gets implemented */
		break;
	default:
		ERROR(ERR_FEATURE_NOT_IMPLEMENTED, input_buffer_combined);
		cursor_used = FALSE; /* Remove this line once this feature gets implemented */
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
