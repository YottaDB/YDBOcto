/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

#define CLEANUP_AND_RETURN_COMMON(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK, CURSOR_YDB_BUFF)                 \
	{                                                                                                           \
		if (NULL != BUFFER) {                                                                               \
			free(BUFFER);                                                                               \
		}                                                                                                   \
		if (NULL != TABLE_BUFFER) {                                                                         \
			free(TABLE_BUFFER);                                                                         \
		}                                                                                                   \
		DELETE_QUERY_PARAMETER_CURSOR_LVN(CURSOR_YDB_BUFF);                                                 \
		if (NULL != QUERY_LOCK) {                                                                           \
			int lclStatus;                                                                              \
                                                                                                                    \
			lclStatus = ydb_lock_decr_s((ydb_buffer_t *)QUERY_LOCK, 1, (ydb_buffer_t *)QUERY_LOCK + 1); \
			YDB_ERROR_CHECK(lclStatus);                                                                 \
		}                                                                                                   \
		OCTO_CFREE(MEMORY_CHUNKS);                                                                          \
	}

#define CLEANUP_AND_RETURN_WITH_SUCCESS(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK, CURSOR_YDB_BUFF)   \
	{                                                                                                   \
		CLEANUP_AND_RETURN_COMMON(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK, CURSOR_YDB_BUFF) \
		return 0;                                                                                   \
	}

#define CLEANUP_AND_RETURN_WITH_ERROR(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK, CURSOR_YDB_BUFF)     \
	{                                                                                                   \
		CLEANUP_AND_RETURN_COMMON(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK, CURSOR_YDB_BUFF) \
		return 1;                                                                                   \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK, CURSOR_YDB_BUFF)       \
	{                                                                                                                \
		YDB_ERROR_CHECK(STATUS);                                                                                 \
		if (YDB_OK != STATUS) {                                                                                  \
			CLEANUP_AND_RETURN_WITH_ERROR(MEMORY_CHUNKS, BUFFER, TABLE_BUFFER, QUERY_LOCK, CURSOR_YDB_BUFF); \
		}                                                                                                        \
	}

#define CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS_SKIP_PARAMETER_LVN(QUERY_LOCK, MEMORY_CHUNKS) \
	{                                                                                  \
		int lclStatus;                                                             \
                                                                                           \
		lclStatus = ydb_lock_decr_s(&QUERY_LOCK[0], 2, &QUERY_LOCK[1]);            \
		YDB_ERROR_CHECK(lclStatus);                                                \
		OCTO_CFREE(MEMORY_CHUNKS);                                                 \
	}

#define CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(QUERY_LOCK, MEMORY_CHUNKS, CURSOR_YDB_BUFF)            \
	{                                                                                           \
		DELETE_QUERY_PARAMETER_CURSOR_LVN(CURSOR_YDB_BUFF);                                 \
		CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS_SKIP_PARAMETER_LVN(QUERY_LOCK, MEMORY_CHUNKS); \
	}

/* Runs a query that has already been read and parsed. Creates a logical and physical plan if necessary. And executes it.
 * Returns
 *	 0 for normal
 *	 1 for error
 *	-1 if query has been canceled.
 */
int run_query(callback_fnptr_t callback, void *parms, PSQL_MessageTypeT msg_type, ParseContext *parse_context) {
	FILE *			  out;
	SqlStatement *		  result;
	SqlValue *		  value;
	bool			  free_memory_chunks;
	char *			  buffer, filename[OCTO_PATH_MAX], routine_name[MAX_ROUTINE_LEN], function_hash[MAX_ROUTINE_LEN];
	ydb_long_t		  cursorId;
	hash128_state_t		  state;
	int			  status;
	size_t			  buffer_size = 0;
	ydb_buffer_t		  query_lock[3], *null_query_lock;
	ydb_string_t		  ci_param1, ci_param2;
	ydb_buffer_t		  cursor_ydb_buff;
	ydb_buffer_t		  schema_global;
	ydb_buffer_t		  octo_global;
	boolean_t		  canceled = FALSE, cursor_used;
	int			  length;
	unsigned int		  ret_value;
	SqlTable *		  table;
	SqlDropTableStatement *	  drop_table;
	char *			  tablename;
	char *			  spcfc_buffer; /* specific buffer (i.e. function-specific or table-specific etc.) */
	ydb_buffer_t		  table_name_buffers[3];
	ydb_buffer_t *		  table_name_buffer;
	SqlFunction *		  function;
	SqlDropFunctionStatement *drop_function;
	char *			  function_name;
	ydb_buffer_t		  function_name_buffers[5];
	ydb_buffer_t *		  function_name_buffer, *function_hash_buffer;
	char			  cursor_buffer[INT64_TO_STRING_MAX];
	char		 pid_buffer[INT64_TO_STRING_MAX]; /* assume max pid is 64 bits even though it is a 4-byte quantity */
	boolean_t	 release_query_lock;
	SqlStatement	 stmt;
	boolean_t	 ok_to_drop, wrapInTp;
	SqlStatementType result_type;

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
	STRCPY_LIT(parse_context->routine, OCTOLIT_NONE, MAX_ROUTINE_LEN);

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
	/* Now that "parse_line()" has been invoked, from this point on, any return path should invoke
	 * DELETE_QUERY_PARAMETER_CURSOR_LVN to cleanup/delete any query parameter related lvn nodes
	 * and avoid lvn buildup across multiple such invocations of "run_query()". This is incorporated in
	 * other macros that are invoked return code paths (e.g. CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS).
	 */
	INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
	if (NULL == result) {
		INFO(INFO_RETURNING_FAILURE, "run_query");
		CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
		return 1;
	}
	result_type = result->type;
	if (config->dry_run || (no_data_STATEMENT == result_type)) {
		CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
		return (YDB_OK != status);
	}
	INFO(INFO_CURSOR, cursor_ydb_buff.buf_addr);
	free_memory_chunks = true; // By default run "octo_cfree(memory_chunks)" at the end

	cursor_used = TRUE; /* By default, assume a cursor was used to execute the query */
	release_query_lock = TRUE;
	switch (result_type) {
	// This effectively means select_STATEMENT, but we have to assign ID's inside this function
	// and so need to propagate them out
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
	case insert_STATEMENT:
		TRACE(INFO_ENTERING_FUNCTION, "hash_canonical_query");
		INVOKE_HASH_CANONICAL_QUERY(state, result, status); /* "state" holds final hash */
		if (0 != status) {
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
		status = generate_routine_name(&state, routine_name, sizeof(routine_name), OutputPlan);
		if (1 == status) {
			ERROR(ERR_PLAN_HASH_FAILED, "");
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
		/* The below call updates "filename" to be the full path including "routine_name" at the end */
		status = get_full_path_of_generated_m_file(filename, sizeof(filename), &routine_name[1]);
		if (status) {
			ERROR(ERR_PLAN_HASH_FAILED, "");
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
		status = emit_physical_or_xref_plan(filename, result, NULL, NULL, NULL);
		if (status) {
			/* Error would have already been issued in "emit_physical_or_xref_plan()" */
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
		if (parse_context->is_extended_query) {
			memcpy(parse_context->routine, routine_name, sizeof(routine_name));
			/* Note: We do not want to do parameter lvn related cleanup as the query is still not completely done
			 * hence using the below macro instead of the usual CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS macro.
			 */
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS_SKIP_PARAMETER_LVN(query_lock, memory_chunks);
			return 0;
		}
		cursorId = atol(cursor_ydb_buff.buf_addr);
		ci_param1.address = routine_name;
		ci_param1.length = sizeof(routine_name);
		/* Currently read-only queries are not wrapped in TP and read-write queries are wrapped in TP. */
		switch (result_type) {
		case table_alias_STATEMENT:
		case set_operation_STATEMENT:
			/* Read-only query */
			wrapInTp = FALSE;
			break;
		default:
			/* Read-write query */
			assert(insert_STATEMENT == result_type);
			wrapInTp = TRUE;
			break;
		}
		// Call the select routine
		status = ydb_ci("_ydboctoselect", cursorId, &ci_param1, (ydb_int_t)wrapInTp);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
		// Check for cancel requests only if running rocto
		if (config->is_rocto) {
			canceled = is_query_canceled(callback);
			if (canceled) {
				CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
				return -1;
			}
		}
		assert(!config->is_rocto || (NULL != parms));
		/* Note: The "callback" function only relies on "stmt.type" so it is okay for other fields to be uninitialized */
		stmt.type = parse_context->command_tag;
		status = (*callback)(&stmt, cursorId, parms, filename, msg_type);
		if (0 != status) {
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
		// Deciding to free the select_STATEMENT etc. must be done by the caller, as they may want to rerun it or send
		// row descriptions hence the decision to not free the memory_chunk below.
		free_memory_chunks = false;
		break;
	case discard_all_STATEMENT: /* DISCARD ALL */
		/* Initialize a few variables to NULL at the start. They are used in the CLEANUP_AND_RETURN_WITH_ERROR and
		 * CLEANUP_AND_RETURN_IF_NOT_YDB_OK macro as parameters (we cannot use NULL literal there due to compile errors).
		 */
		buffer = NULL;
		spcfc_buffer = NULL;
		null_query_lock = NULL;
		/* Now release the shared query lock and get an exclusive query lock to do changes to plans/xrefs/triggers */
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock, &cursor_ydb_buff);
		/* Wait 10 seconds for the exclusive DDL change lock */
		status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock, &cursor_ydb_buff);
		/* Call an M routine to discard all plans, xrefs and triggers associated with all tables in Octo.
		 * Cannot use SimpleAPI for at least one step (deleting the triggers). Hence using M for all the steps.
		 */
		status = ydb_ci("_ydboctoDiscardAll");
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset a few variables to NULL as those
			 * parts of the cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			assert(NULL == spcfc_buffer);
			assert(NULL == null_query_lock);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock,
							 &cursor_ydb_buff);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
		break;
	case drop_table_STATEMENT:   /* DROP TABLE */
	case create_table_STATEMENT: /* CREATE TABLE */
		/* Note that DROP TABLE is very similar to DROP FUNCTION, and changes to either may need to be
		 * reflected in the other.
		 */
		/* Note that CREATE TABLE is very similar to CREATE FUNCTION, and changes to either may need to be
		 * reflected in the other.
		 */
		table_name_buffer = &table_name_buffers[0];
		/* Initialize a few variables to NULL at the start. They are really used much later but any calls to
		 * CLEANUP_AND_RETURN_WITH_ERROR and CLEANUP_AND_RETURN_IF_NOT_YDB_OK before then need this so they skip freeing
		 * this.
		 */
		buffer = NULL;
		spcfc_buffer = NULL;
		null_query_lock = NULL;
		/* Now release the shared query lock and get an exclusive query lock to do DDL changes */
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock, &cursor_ydb_buff);
		/* Wait 10 seconds for the exclusive DDL change lock */
		status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock, &cursor_ydb_buff);
		/* Note: "null_query_lock" used above as we do not have any query lock to release
		 * at this point since query lock grab failed.
		 */
		/* First get a ydb_buffer_t of the table name into "table_name_buffer" */
		if (drop_table_STATEMENT == result_type) {
			UNPACK_SQL_STATEMENT(drop_table, result, drop_table);
			UNPACK_SQL_STATEMENT(value, drop_table->table_name, value);
			tablename = value->v.reference;
			table = find_table(tablename);
			if (NULL == table) {
				/* Table to be dropped does not exist. */
				if (FALSE == drop_table->if_exists_specified) {
					/* The DROP TABLE statement does not specify IF EXISTS. Issue error. */
					ERROR(ERR_CANNOT_DROP_TABLE, tablename);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				} else {
					/* The DROP TABLE statement specifies IF EXISTS. Issue info message. */
					INFO(INFO_TABLE_DOES_NOT_EXIST, tablename);
					CLEANUP_AND_RETURN_WITH_SUCCESS(memory_chunks, buffer, spcfc_buffer, query_lock,
									&cursor_ydb_buff);
				}
			}
			ok_to_drop = TRUE;
		} else {
			UNPACK_SQL_STATEMENT(table, result, create_table);
			UNPACK_SQL_STATEMENT(value, table->tableName, value);
			tablename = value->v.reference;
			/* If auto load of octo-seed.sql is in progress, a CREATE TABLE must do a DROP TABLE. */
			ok_to_drop = config->in_auto_load_octo_seed;
		}
		YDB_STRING_TO_BUFFER(tablename, table_name_buffer);
		if (ok_to_drop) {
			/* DROP TABLE */
			char	     tableGVNAME[YDB_MAX_IDENT + 1];
			ydb_buffer_t gvname_buff;

			/* Check if OIDs were created for this table.
			 * If so, delete those nodes from the catalog now that this table is going away.
			 */
			status = delete_table_from_pg_class(table_name_buffer);
			if (0 != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			/* Call an M routine to discard all plans, xrefs and triggers associated with the table being
			 * created/dropped. Cannot use SimpleAPI for at least one step (deleting the triggers). Hence using M for
			 * all the steps.
			 */
			if (table->readwrite) {
				/* This is a READWRITE table. DROP TABLE should also KILL the gvn corresponding to this table. */
				SqlOptionalKeyword *keyword;
				char *		    gvname, *firstsub;

				UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
				UNPACK_SQL_STATEMENT(value, keyword->v, value);
				gvname = value->v.reference;
				firstsub = strchr(gvname, '(');
				if (NULL == firstsub) {
					/* Not sure how an unsubscripted gvn can be specified in GLOBAL. But handle it anyways. */
					assert(FALSE);
					YDB_STRING_TO_BUFFER(gvname, &gvname_buff);
				} else {
					memcpy(tableGVNAME, gvname, firstsub - gvname);
					tableGVNAME[firstsub - gvname] = '\0';
					YDB_STRING_TO_BUFFER(tableGVNAME, &gvname_buff);
				}
			} else {
				YDB_STRING_TO_BUFFER("", &gvname_buff);
			}
			ci_param1.address = table_name_buffer->buf_addr;
			ci_param1.length = table_name_buffer->len_used;
			ci_param2.address = gvname_buff.buf_addr;
			ci_param2.length = gvname_buff.len_used;
			status = ydb_ci("_ydboctoDiscardTable", &ci_param1, &ci_param2);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			/* Now that OIDs and plan nodes have been cleaned up, dropping the table is a simple
			 *	KILL ^%ydboctoschema(TABLENAME)
			 */
			status = ydb_delete_s(&schema_global, 1, table_name_buffer, YDB_DEL_TREE);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			/* Drop the table from the local cache */
			status = drop_schema_from_local_cache(table_name_buffer, TableSchema, NULL);
			if (YDB_OK != status) {
				/* YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()" */
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
		}
		if (create_table_STATEMENT == result_type) {
			/* CREATE TABLE case. More processing needed. */
			int	  text_table_defn_length;
			SqlTable *sql_table;

			if (!ok_to_drop) {
				/* Check if table already exists */
				sql_table = find_table(tablename);
				if (NULL != sql_table) {
					/* Table already exists. */
					if (FALSE == table->if_not_exists_specified) {
						/* The CREATE TABLE statement does not specify IF NOT EXISTS. Issue error. */
						ERROR(ERR_CANNOT_CREATE_TABLE, tablename);
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					} else {
						/* The CREATE TABLE statement specifies IF NOT EXISTS. Issue info message. */
						INFO(INFO_TABLE_ALREADY_EXISTS, tablename);
						CLEANUP_AND_RETURN_WITH_SUCCESS(memory_chunks, buffer, spcfc_buffer, query_lock,
										&cursor_ydb_buff);
					}
				}
			}
			out = open_memstream(&buffer, &buffer_size);
			assert(out);
			text_table_defn_length = emit_create_table(out, result);
			fclose(out); // at this point "buffer" and "buffer_size" are usable
			if (0 > text_table_defn_length) {
				// Error messages for the negative status would already have been issued in
				// "emit_create_table"
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			INFO(INFO_TEXT_REPRESENTATION,
			     buffer); /* print the converted text representation of the CREATE TABLE command */

			/* Store the text representation of the CREATE TABLE statement:
			 *	^%ydboctoschema(table_name,OCTOLIT_TEXT)
			 */
			status = store_table_definition(table_name_buffers, buffer, text_table_defn_length, TRUE);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			free(buffer);
			buffer = NULL; /* So CLEANUP_AND_RETURN_WITH_ERROR* macro calls below do not try "free(buffer)" */
			/* First store table name in catalog. As we need that OID to store in the binary table definition.
			 * The below call also sets table->oid which is needed before the call to "compress_statement" as
			 * that way the oid also gets stored in the binary table definition.
			 */
			status = store_table_in_pg_class(table, table_name_buffer);
			/* Cannot use CLEANUP_AND_RETURN_IF_NOT_YDB_OK macro here because the above function could set
			 * status to 1 to indicate an error (not necessarily a valid YDB_ERR_* code). In case it is a
			 * YDB error code, the YDB_ERROR_CHECK call would have already been done inside "store_table_in_pg_class"
			 * so all we need to do here is check if status is not 0 (aka YDB_OK) and if so invoke
			 * CLEANUP_AND_RETURN_WITH_ERROR.
			 */
			if (YDB_OK != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			compress_statement(result, &spcfc_buffer, &length); /* Sets "spcfc_buffer" to "malloc"ed storage */
			assert(NULL != spcfc_buffer);
			status = store_table_definition(table_name_buffers, spcfc_buffer, length, FALSE);
			free(spcfc_buffer);  /* free buffer that was "malloc"ed in "compress_statement" */
			spcfc_buffer = NULL; /* Now that we did a "free", reset it to NULL */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		}
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset few variables to NULL as those parts of the
			 * cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			spcfc_buffer = NULL;
			assert(NULL == null_query_lock);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock,
							 &cursor_ydb_buff);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
		break;			    /* OCTO_CFREE(memory_chunks) will be done after the "break" */
	case drop_function_STATEMENT:	    /* DROP FUNCTION */
	case create_function_STATEMENT:	    /* CREATE FUNCTION */
		/* Note that DROP FUNCTION is very similar to DROP TABLE, and changes to either may need to be
		 * reflected in the other.
		 */
		/* Note that CREATE FUNCTION is very similar to CREATE TABLE, and changes to either may need to be
		 * reflected in the other.
		 */
		function_name_buffer = &function_name_buffers[1];
		/* Initialize a few variables to NULL at the start. They are really used much later but any calls to
		 * CLEANUP_AND_RETURN_WITH_ERROR and CLEANUP_AND_RETURN_IF_NOT_YDB_OK before then need this so they skip freeing
		 * this.
		 */
		buffer = NULL;
		spcfc_buffer = NULL;
		null_query_lock = NULL;
		// Now release the shared query lock and get an exclusive query lock to do DDL changes
		status = ydb_lock_decr_s(&query_lock[0], 2, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock, &cursor_ydb_buff);
		// Wait 10 seconds for the exclusive DDL change lock
		status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &query_lock[0], 1, &query_lock[1]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock, &cursor_ydb_buff);
		/* Note: "null_query_lock" parameter used above as we do not have any query lock to release
		 * at this point since query lock grab failed.
		 */
		/* First, get a ydb_buffer_t of the function name into "function_name_buffer" */
		if (create_function_STATEMENT == result_type) {
			UNPACK_SQL_STATEMENT(function, result, create_function);
			UNPACK_SQL_STATEMENT(value, function->function_name, value);
		} else {
			UNPACK_SQL_STATEMENT(drop_function, result, drop_function);
			UNPACK_SQL_STATEMENT(value, drop_function->function_name, value);
		}
		function_name = value->v.reference;
		// Then hash the function parameters to determine which function definition is to be CREATEd or DROPed
		INVOKE_HASH_CANONICAL_QUERY(state, result, status); /* "state" holds final hash */
		if (0 != status) {
			CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		}
		status = generate_routine_name(&state, function_hash, sizeof(function_hash), FunctionHash);
		if (1 == status) {
			CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		}
		function_hash_buffer = &function_name_buffers[2];
		YDB_STRING_TO_BUFFER(function_hash, function_hash_buffer);
		// Add function hash to parse tree for later addition to logical plan
		if (create_function_STATEMENT == result_type) {
			SQL_STATEMENT(function->function_hash, value_STATEMENT);
			MALLOC_STATEMENT(function->function_hash, value, SqlValue);
			UNPACK_SQL_STATEMENT(value, function->function_hash, value);
			value->v.string_literal = octo_cmalloc(memory_chunks, function_hash_buffer->len_used + 1);
			memcpy(value->v.string_literal, function_hash, function_hash_buffer->len_used);
			value->v.string_literal[function_hash_buffer->len_used] = '\0'; /* null terminate */
			value->type = FUNCTION_HASH;
		} else {
			function = NULL; /* to avoid false [-Wmaybe-uninitialized] warnings from compiler */
		}
		YDB_STRING_TO_BUFFER(function_name, function_name_buffer);

		// Initialize buffers for accessing relevant function GVNs
		YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
		YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_name_buffers[0]);
		function_name_buffers[4].buf_addr = filename;
		function_name_buffers[4].len_used = 0;
		function_name_buffers[4].len_alloc = sizeof(filename);

		/* Check if function with computed hash exists already or not */
		status = ydb_data_s(&octo_global, 3, &function_name_buffers[0], &ret_value);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		if (drop_function_STATEMENT == result_type) {
			/* DROP FUNCTION */
			if (0 == ret_value) {
				char fn_name[1024];

				/* Function does not already exist. */
				get_function_name_and_parmtypes(fn_name, sizeof(fn_name), function_name,
								drop_function->parameter_type_list);
				if (FALSE == drop_function->if_exists_specified) {
					/* The DROP FUNCTION statement does not specify IF EXISTS. Issue error. */
					ERROR(ERR_CANNOT_DROP_FUNCTION, fn_name);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				} else {
					/* The DROP FUNCTION statement specifies IF EXISTS. Issue info message. */
					INFO(INFO_FUNCTION_DOES_NOT_EXIST, fn_name);
					CLEANUP_AND_RETURN_WITH_SUCCESS(memory_chunks, buffer, spcfc_buffer, query_lock,
									&cursor_ydb_buff);
				}
			}
			ok_to_drop = TRUE;
		} else {
			/* CREATE FUNCTION */
			/* If auto load of octo-seed.sql is in progress, a CREATE FUNCTION must do a DROP FUNCTION. */
			ok_to_drop = config->in_auto_load_octo_seed;
			if (!ok_to_drop) {
				if (0 != ret_value) {
					char fn_name[1024];

					/* Function already exists. */
					get_function_name_and_parmtypes(fn_name, sizeof(fn_name), function_name,
									function->parameter_type_list);
					if (FALSE == function->if_not_exists_specified) {
						/* The CREATE FUNCTION statement does not specify IF NOT EXISTS. Issue error. */
						ERROR(ERR_CANNOT_CREATE_FUNCTION, fn_name);
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					} else {
						/* The CREATE FUNCTION statement specifies IF NOT EXISTS. Issue info message. */
						INFO(INFO_FUNCTION_ALREADY_EXISTS, fn_name);
						CLEANUP_AND_RETURN_WITH_SUCCESS(memory_chunks, buffer, spcfc_buffer, query_lock,
										&cursor_ydb_buff);
					}
				}
			} else {
				/* It is okay for a function that is being created (using "CREATE FUNCTION" in "octo-seed.sql")
				 * to already exist or not. No errors should be issued in either case. Handle it accordingly.
				 */
				ok_to_drop = (0 != ret_value); /* DROP FUNCTION only if it exists */
			}
		}
		if (ok_to_drop) {
			/* DROP FUNCTION explicitly requested or implicitly assumed as part of auto load of "octo-seed.sql" */
			status = delete_function_from_pg_proc(function_name_buffer, function_hash_buffer);
			if (0 != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			/* Call an M routine to discard all plans associated with the function being created/dropped */
			ci_param1.address = function_name_buffer->buf_addr;
			ci_param1.length = function_name_buffer->len_used;
			ci_param2.address = function_hash_buffer->buf_addr;
			ci_param2.length = function_hash_buffer->len_used;
			status = ydb_ci("_ydboctoDiscardFunction", &ci_param1, &ci_param2);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);

			// Drop the function from the local cache
			status = drop_schema_from_local_cache(function_name_buffer, FunctionSchema, function_hash_buffer);
			if (YDB_OK != status) {
				// YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()"
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
		}
		if (create_function_STATEMENT == result_type) {
			int text_function_defn_length;

			/* CREATE FUNCTION */
			out = open_memstream(&buffer, &buffer_size);
			assert(out);
			text_function_defn_length = emit_create_function(out, result);
			fclose(out); // at this point "buffer" and "buffer_size" are usable
			if (0 > text_function_defn_length) {
				/* Error messages for the non-zero status would already have been issued in
				 * "emit_create_function"
				 */
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			/* Print the converted text representation of the CREATE TABLE command */
			INFO(INFO_TEXT_REPRESENTATION, buffer);
			/* Store the text representation of the CREATE FUNCTION statement:
			 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT)
			 */
			status = store_function_definition(function_name_buffers, buffer, text_function_defn_length, TRUE);
			free(buffer);
			buffer = NULL; // So CLEANUP_AND_RETURN_WITH_ERROR* macro calls below do not try "free(buffer)"

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
			 * so all we need to do here is check if status is not 0 (aka YDB_OK) and if so invoke
			 * CLEANUP_AND_RETURN_WITH_ERROR.
			 */
			if (YDB_OK != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}

			/* Now that we know there are no too-many-parameter errors in ths function, we can safely go ahead
			 * with setting the function related gvn in the database.
			 */
			compress_statement(result, &spcfc_buffer, &length); /* Sets "spcfc_buffer" to "malloc"ed storage */
			assert(NULL != spcfc_buffer);
			status = store_function_definition(function_name_buffers, spcfc_buffer, length, FALSE);
			free(spcfc_buffer);  /* free buffer that was "malloc"ed in "compress_statement" */
			spcfc_buffer = NULL; /* Now that we did a "free", reset it to NULL */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		}
		status = ydb_lock_decr_s(&query_lock[0], 1, &query_lock[1]); /* Release exclusive query lock */
		if (YDB_OK != status) {
			/* Signal an error using the standard macro but reset few variables to NULL as those parts of the
			 * cleanup should not be done in this part of the code.
			 */
			assert(NULL == buffer);
			spcfc_buffer = NULL;
			assert(NULL == null_query_lock);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, null_query_lock,
							 &cursor_ydb_buff);
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
		status = (*callback)(result, cursorId, parms, NULL, msg_type);
		if (YDB_OK != status) {
			CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, NULL, NULL, query_lock, &cursor_ydb_buff);
		}
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
	if (cursor_used && !parse_context->skip_cursor_cleanup)
		DELETE_QUERY_PARAMETER_CURSOR_LVN(&cursor_ydb_buff);
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
