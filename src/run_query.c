/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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
	FILE	     *out;
	SqlStatement *result;
	SqlValue     *value;
	boolean_t     free_memory_chunks;
	// + 1 for NULL terminator
	char	       *buffer, filename[OCTO_PATH_MAX + 1], routine_name[MAX_ROUTINE_LEN + 1], function_hash[MAX_ROUTINE_LEN + 1];
	ydb_long_t	cursorId;
	hash128_state_t state;
	int		status;
	size_t		buffer_size = 0;
	ydb_buffer_t	query_lock[3], *null_query_lock;
	ydb_string_t	ci_param1, ci_param2;
	ydb_buffer_t	cursor_ydb_buff;
	ydb_buffer_t	schema_global;
	ydb_buffer_t	octo_global;
	boolean_t	canceled = FALSE, cursor_used;
	int		length;
	unsigned int	ret_value;
	SqlTable       *table;
	SqlDropTableStatement	 *drop_table;
	char			 *tablename;
	char			 *spcfc_buffer; /* specific buffer (i.e. function-specific or table-specific etc.) */
	ydb_buffer_t		  table_name_buffers[3];
	ydb_buffer_t		 *table_name_buffer;
	SqlFunction		 *function;
	SqlDropFunctionStatement *drop_function;
	char			 *function_name;
	ydb_buffer_t		  function_name_buffers[5];
	ydb_buffer_t		 *function_name_buffer, *function_hash_buffer;
	char			  cursor_buffer[INT64_TO_STRING_MAX];
	char		    pid_buffer[INT64_TO_STRING_MAX]; /* assume max pid is 64 bits even though it is a 4-byte quantity */
	boolean_t	    release_query_lock;
	SqlStatement	    stmt;
	boolean_t	    ok_to_drop, wrapInTp;
	SqlStatementType    result_type;
	SqlDisplayRelation *display_relation;
	SqlStatement	   *table_stmt;

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

	// Kill view's cache created for previous query
	INIT_VIEW_CACHE_FOR_CURRENT_QUERY(config->global_names.loadedschemas, status);
	if (YDB_OK != status) {
		YDB_ERROR_CHECK(status);
	}

	result = parse_line(parse_context);
	/* Now that "parse_line()" has been invoked, from this point on, any return path should invoke
	 * DELETE_QUERY_PARAMETER_CURSOR_LVN to cleanup/delete any query parameter related lvn nodes
	 * and avoid lvn buildup across multiple such invocations of "run_query()". This is incorporated in
	 * other macros that are invoked return code paths (e.g. CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS).
	 */
	INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
	if (config->octo_print_query) {
		fprintf(stdout, "OCTO> %.*s\n", cur_input_index - old_input_index, input_buffer_combined + old_input_index);
		/* It is possible error output of this query goes to stderr. In that case, it is possible for the two
		 * output streams to get mixed up giving confusing results. Therefore, flush stdout right away.
		 */
		fflush(stdout);
	}
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
	free_memory_chunks = TRUE; // By default run "octo_cfree(memory_chunks)" at the end

	cursor_used = TRUE; /* By default, assume a cursor was used to execute the query */
	release_query_lock = TRUE;
	switch (result_type) {
	case display_relation_STATEMENT:
		UNPACK_SQL_STATEMENT(display_relation, result, display_relation);
		switch (display_relation->type) {
		case DISPLAY_TABLE_RELATION:
			/* "\d tablename" : Describe/Display a specific table */
			status = describe_table_or_view_name(display_relation->table_name);
			if (0 != status) {
				CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
				return status;
			}
			break;
		case DISPLAY_ALL_RELATION:
			/* "\d" : Describe/Display all relations/tables/views */
		case DISPLAY_ALL_VIEW_RELATION:
			/* "\dv": Describe/Display all views */
			result = get_display_relation_query_stmt(display_relation->type, parse_context);
			assert(table_alias_STATEMENT == result->type);
			result_type = table_alias_STATEMENT;
			/* `result` retrieved from the above call will be a table_alias_STATEMENT.
			 * Execute the code in table_alias_STATEMENT switch case below by falling through.
			 */
			break;
		default:
			assert(FALSE);
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return -1;
			break;
		}
		if (DISPLAY_TABLE_RELATION == display_relation->type) {
			/* The switch block above for DISPLAY_TABLE_RELATION has executed successfully.
			 * There is nothing else to do for this case. Issue a `break` so that query completion
			 * related logging can happen at the end of this function.
			 */
			break;
		}
		/* fall through */
	case table_alias_STATEMENT:
		// This effectively means select_STATEMENT, but we have to assign ID's inside this function
		// and so need to propagate them out
	case set_operation_STATEMENT:
	case insert_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
		TRACE(INFO_ENTERING_FUNCTION, "hash_canonical_query");
		INVOKE_HASH_CANONICAL_QUERY(state, result, status); /* "state" holds final hash */
		if (0 != status) {
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
		generate_name_type(OutputPlan, &state, 0, routine_name, sizeof(routine_name));
		/* The below call updates "filename" to be the full path including "routine_name" at the end */
		// - 1 don't include null terminator in size
		status = get_full_path_of_generated_m_file(filename, sizeof(filename) - 1, &routine_name[1]);
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
			// - 1 don't include null terminator in size
			memcpy(parse_context->routine, routine_name, sizeof(routine_name) - 1);
			/* Note: We do not want to do parameter lvn related cleanup as the query is still not completely done
			 * hence using the below macro instead of the usual CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS macro.
			 */
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS_SKIP_PARAMETER_LVN(query_lock, memory_chunks);
			return 0;
		}
		cursorId = atol(cursor_ydb_buff.buf_addr);
		ci_param1.address = routine_name;
		ci_param1.length = sizeof(routine_name) - 1; // don't include null terminator
		/* Currently read-only queries are not wrapped in TP and read-write queries are wrapped in TP. */
		switch (result_type) {
		case table_alias_STATEMENT:
		case set_operation_STATEMENT:
			/* Read-only query */
			wrapInTp = FALSE;
			break;
		default:
			/* Read-write query */
			assert((insert_STATEMENT == result_type) || (delete_from_STATEMENT == result_type)
			       || (update_STATEMENT == result_type));
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
		free_memory_chunks = FALSE;
		break;
	case discard_xrefs_STATEMENT: /* DISCARD XREFS */
	case discard_all_STATEMENT:   /* DISCARD ALL */
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
		if (result_type == discard_all_STATEMENT) {
			/* Call an M routine to discard all plans, xrefs and triggers associated with all tables in Octo.
			 * Cannot use SimpleAPI for at least one step (deleting the triggers). Hence using M for all the steps.
			 */
			status = ydb_ci("_ydboctoDiscardAll");
		} else {
			assert(result_type == discard_xrefs_STATEMENT);
			status = ydb_ci("_ydboctoDiscardXREFS");
		}
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
	case truncate_table_STATEMENT:; /* TRUNCATE TABLE */
		ydb_tpfnptr_t tpfn;
		tpfn = (ydb_tpfnptr_t)&truncate_table_tp_callback_fn;
		status = ydb_tp_s(tpfn, result, NULL, 0, NULL);
		if (YDB_OK == status) {
			PRINT_COMMAND_TAG(TRUNCATE_TABLE_COMMAND_TAG);
		} else {
			assert(YDB_TP_ROLLBACK == status);
			CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
			return 1;
		}
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
		if ((drop_table_STATEMENT == result_type) && !config->in_auto_load_octo_seed) {
			UNPACK_SQL_STATEMENT(drop_table, result, drop_table);
			UNPACK_SQL_STATEMENT(value, drop_table->table_name, value);
			tablename = value->v.reference;
			table_stmt = find_view_or_table(tablename);
			if (NULL == table_stmt) {
				/* Table to be dropped does not exist. */
				if (FALSE == drop_table->if_exists_specified) {
					/* The DROP TABLE statement does not specify IF EXISTS. Issue error. */
					ERROR(ERR_CANNOT_DROP_TABLE, tablename);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				} else {
					/* The DROP TABLE statement specifies IF EXISTS. Issue info message. */
					INFO(INFO_TABLE_DOES_NOT_EXIST, tablename);
					PRINT_COMMAND_TAG(DROP_TABLE_COMMAND_TAG);
					CLEANUP_AND_RETURN_WITH_SUCCESS(memory_chunks, buffer, spcfc_buffer, query_lock,
									&cursor_ydb_buff);
				}
			} else if (!config->is_auto_upgrade_octo929) {
				/* Check if a View relies on this table. If so, disallow the DROP TABLE.
				 * We don't want to issue this error in case of the YDBOcto#929 auto upgrade (where it is
				 * possible for such scenarios to arise but it is okay because we are going to recreate all
				 * tables/functions/views and so all dependencies are going to be recomputed. Hence the
				 * "else if (!config->is_auto_upgrade_octo929)" check above.
				 */
				if (create_table_STATEMENT != table_stmt->type) {
					// issue an error and clean up
					ERROR(ERR_WRONG_TYPE, tablename, OCTOLIT_TABLE);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				}
				/* If a View relies on this table, we would see gvn nodes of the following form
				 * exist in the database.
				 *	^ydboctoocto("tableviewdependency","NAMES","V1")=""
				 * where
				 *	"NAMES" = table name
				 *	"V1" = view name
				 * Therefore check if ^%ydboctoocto("tableviewdependency","NAMES",*)
				 * nodes exist and if so issue an error.
				 */
				// gvn_subs[0] -> ^%ydboctoocto
				// gvn_subs[1] -> "tableviewdependency"
				// gvn_subs[2] -> table_name
				ydb_buffer_t gvn_subs[4];
				YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
				YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLEVIEWDEPENDENCY, &gvn_subs[1]);
				YDB_STRING_TO_BUFFER(tablename, &gvn_subs[2]);

				char view_name_buffer[OCTO_MAX_IDENT + 1];
				gvn_subs[3].buf_addr = view_name_buffer;
				gvn_subs[3].len_alloc = sizeof(view_name_buffer) - 1; /* reserve 1 byte for null terminator */
				gvn_subs[3].len_used = 0;
				status = ydb_subscript_next_s(&gvn_subs[0], 3, &gvn_subs[1], &gvn_subs[3]);
				if (YDB_ERR_NODEEND != status) {
					/* There is atleast one view which relies on this table. Issue error */
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					}
					gvn_subs[3].buf_addr[gvn_subs[3].len_used] = '\0'; /* null terminate view name */
					ERROR(ERR_DROP_TABLE_DEPENDS_ON_VIEW, tablename, gvn_subs[3].buf_addr);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				}
			}
			UNPACK_SQL_STATEMENT(table, table_stmt, create_table);
			ok_to_drop = TRUE;
		} else {
			if (drop_table_STATEMENT == result_type) {
				UNPACK_SQL_STATEMENT(drop_table, result, drop_table);
				UNPACK_SQL_STATEMENT(value, drop_table->table_name, value);
				assert(OPTIONAL_KEEPDATA == drop_table->drop_data_retention);
				table = NULL; /* To avoid false [-Wmaybe-uninitialized] warning */
			} else {
				UNPACK_SQL_STATEMENT(table, result, create_table);
				UNPACK_SQL_STATEMENT(value, table->tableName, value);
				drop_table = NULL; /* To avoid false [-Wmaybe-uninitialized] warning */
			}
			tablename = value->v.reference;
			/* If auto load of octo-seed.sql is in progress, a CREATE TABLE must do a DROP TABLE. */
			ok_to_drop = config->in_auto_load_octo_seed;
		}
		/* First get a ydb_buffer_t of the table name into "table_name_buffer" */
		YDB_STRING_TO_BUFFER(tablename, table_name_buffer);
		if (config->is_auto_upgrade_octo929) {
			/* Note down that a CREATE TABLE or DROP TABLE with this table name happened.
			 * Later used to avoid duplicate CREATE TABLE/DROP TABLE calls during the auto upgrade process.
			 */
			ydb_buffer_t ydbocto929, table_subs[2];
			char	     subs0_buff[INT32_TO_STRING_MAX];

			YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTO929, &ydbocto929);
			table_subs[0].buf_addr = subs0_buff;
			table_subs[0].len_alloc = sizeof(subs0_buff);
			table_subs[0].len_used = snprintf(table_subs[0].buf_addr, table_subs[0].len_alloc, "%d", result_type);
			table_subs[1] = *table_name_buffer;
			status = ydb_set_s(&ydbocto929, 2, &table_subs[0], NULL);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		}
		if (ok_to_drop) {
			/* DROP TABLE */
			/* If we are doing auto load of octo-seed.sql and encountering DROP TABLE commands etc., we should not
			 * be deleting any globals corresponding to the UNIQUE constraints as they hold constraint enforcement
			 * information.
			 */
			assert(config->in_auto_load_octo_seed || config->is_auto_upgrade_octo929 || (NULL != table));
			/* The "(NULL != table)" check below is not needed (due to the assert above) but is needed to
			 * avoid false [clang-analyzer-core.NullDereference] warnings from clang-tidy.
			 */
			if (!config->in_auto_load_octo_seed && !config->is_auto_upgrade_octo929 && (NULL != table)) {
				/* Delete globals that maintained UNIQUE constraints (if any) for this table */
				SqlColumn *start_column, *cur_column;
				UNPACK_SQL_STATEMENT(start_column, table->columns, column);
				cur_column = start_column;
				do {
					SqlOptionalKeyword *start_keyword, *cur_keyword;
					UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
					cur_keyword = start_keyword;
					do {
						switch (cur_keyword->keyword) {
						case UNIQUE_CONSTRAINT:;
							SqlConstraint *constraint;
							UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
							UNPACK_SQL_STATEMENT(value, constraint->v.uniq_gblname, value);

							ydb_buffer_t uniq_gblname;
							YDB_STRING_TO_BUFFER(value->v.string_literal, &uniq_gblname);
							status = ydb_delete_s(&uniq_gblname, 0, NULL, YDB_DEL_TREE);
							CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer,
											 spcfc_buffer, query_lock,
											 &cursor_ydb_buff);
							break;
						default:
							break;
						}
						cur_keyword = cur_keyword->next;
					} while (cur_keyword != start_keyword);
					cur_column = cur_column->next;
				} while (cur_column != start_column);
			}
			/* Check if OIDs were created for this table.
			 * If so, delete those nodes from the catalog now that this table is going away.
			 */
			status = delete_table_or_view_from_pg_class(table_name_buffer);
			if (0 != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}

			char tableGVNAME[YDB_MAX_IDENT + 2]; // + 2: One for ^, one for null byte. YDB_MAX_IDENT does not include ^.
			ydb_buffer_t	     gvname_buff;
			enum OptionalKeyword retention = NO_KEYWORD;
			if (drop_table_STATEMENT == result_type) {
				retention = drop_table->drop_data_retention;
				assert((OPTIONAL_KEEPDATA == retention) || (NULL != table));
			}
			/* The "(NULL != table)" check below is not needed (due to the assert above) but is needed to
			 * avoid false [clang-analyzer-core.NullDereference] warnings from clang-tidy.
			 */
			if ((OPTIONAL_KEEPDATA != retention) && (NULL != table) && table->readwrite) {
				/* If we are doing auto-load of octo-seed.sql, we have not yet upgraded the binary table
				 * definitions. Therefore, we should never come here as the below macro call tries to use
				 * the "table->keyword" structure and it won't be valid until the binary definition is upgraded.
				 * This is taken care of by ensuring all "DROP TABLE" commands in "octo-seed.sql" have KEEPDATA
				 * in them. Hence the below assert.
				 */
				assert(!config->in_auto_load_octo_seed);
				POPULATE_GVN_BUFFER_FROM_TABLE(gvname_buff, table, tableGVNAME);
			} else {
				YDB_STRING_TO_BUFFER("", &gvname_buff);
			}
			/* Call an M routine to discard all plans, xrefs and triggers associated with the table being
			 * created/dropped. Cannot use SimpleAPI for at least one step (deleting the triggers). Hence using M for
			 * all the steps.
			 */
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
			if (!config->in_auto_load_octo_seed && !config->is_auto_upgrade_octo929) {
				/* Drop the table from the local cache */
				status = drop_schema_from_local_cache(table_name_buffer, TableSchema, NULL);
				if (YDB_OK != status) {
					/* YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()" */
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				}
			}
		}
		if (create_table_STATEMENT == result_type) {
			/* CREATE TABLE case. More processing needed. */
			int	      text_table_defn_length;
			SqlStatement *sql_table_stmt;

			if (!ok_to_drop) {
				/* Check if table already exists */
				sql_table_stmt = find_view_or_table(tablename);
				if (NULL != sql_table_stmt) {
					/* Table already exists. */
					if (FALSE == table->if_not_exists_specified) {
						/* The CREATE TABLE statement does not specify IF NOT EXISTS. Issue error. */
						ERROR(ERR_CANNOT_CREATE_TABLE, tablename);
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					} else {
						/* The CREATE TABLE statement specifies IF NOT EXISTS. Issue info message. */
						INFO(INFO_TABLE_ALREADY_EXISTS, tablename);
						PRINT_COMMAND_TAG(CREATE_TABLE_COMMAND_TAG);
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
			status = store_table_or_view_in_pg_class(result, table_name_buffer);
			/* Cannot use CLEANUP_AND_RETURN_IF_NOT_YDB_OK macro here because the above function could set
			 * status to 1 to indicate an error (not necessarily a valid YDB_ERR_* code). In case it is a
			 * YDB error code, the YDB_ERROR_CHECK call would have already been done inside "store_table_in_pg_class"
			 * so all we need to do here is check if status is not 0 (aka YDB_OK) and if so invoke
			 * CLEANUP_AND_RETURN_WITH_ERROR.
			 */
			if (YDB_OK != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			compress_statement(result, &spcfc_buffer, &length, FALSE); /* Sets "spcfc_buffer" to "malloc"ed storage */
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
		if (drop_table_STATEMENT == result_type) {
			PRINT_COMMAND_TAG(DROP_TABLE_COMMAND_TAG);
		} else {
			PRINT_COMMAND_TAG(CREATE_TABLE_COMMAND_TAG);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
		break;			    /* OCTO_CFREE(memory_chunks) will be done after the "break" */
	case drop_view_STATEMENT:;	    /* DROP VIEW */
	case create_view_STATEMENT:;	    /* CREATE VIEW */
		/* Complete view definition in case this is a create_view_STATEMENT.
		 * This helps to avoid asterisk related parsing issues. Refer to the following comment for more details
		 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1378#note_1380319421
		 * Similar code exists in src/auto_upgrade_binary_table_or_view_definition_helper.c any change here needs to be
		 * done there as well
		 */
		if (create_view_STATEMENT == result_type) {
			result = view_definition(result, parse_context);
			if (NULL == result) {
				CLEANUP_QUERY_LOCK_AND_MEMORY_CHUNKS(query_lock, memory_chunks, &cursor_ydb_buff);
				return 1;
			}
		}
		/* Following code is similar to CREATE TABLE and DROP TABLE code block.
		 * Any changes to either code block may need to be reflected in the other.
		 */
		ydb_buffer_t  view_name_buffers[3];
		ydb_buffer_t *view_name_buffer;
		view_name_buffer = &view_name_buffers[0];
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
		/* Note: "null_query_lock" used above as we do not have any query lock to release as this point
		 * since query lock grab failed.
		 */
		/* First get a ydb_buffer_t of the view name into "view_name_buffer" */
		SqlDropViewStatement *drop_view;
		SqlView		     *view;
		char		     *viewname;
		boolean_t	      is_view = FALSE, node_found = TRUE;
		if (drop_view_STATEMENT == result_type) {
			UNPACK_SQL_STATEMENT(drop_view, result, drop_view);
			UNPACK_SQL_STATEMENT(value, drop_view->view_name, value);
			viewname = value->v.reference;

			/* Determine if a node with the given name exists in ^%ydboctoschema.
			 * If it does check if its a view by looking at the value stored.
			 * If it is a view the node will be of the following form:
			 * A view will have ^%ydboctoschema(view_name)=OCTOLIT_VIEWS
			 */
			ydb_buffer_t schema_global;
			YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);

			ydb_buffer_t sub;
			YDB_STRING_TO_BUFFER(viewname, &sub);

			ydb_buffer_t node_value;
			char	     node_value_buff[OCTO_MAX_IDENT + 1];
			node_value.buf_addr = node_value_buff;
			node_value.len_alloc = sizeof(node_value_buff) - 1; /* reserve 1 byte for null terminator */
			status = ydb_get_s(&schema_global, 1, &sub, &node_value);
			switch (status) {
			case YDB_OK:
				// A node of the given name exists in ^%ydboctoschema
				// Check if the value stored is a view by looking at its value.
				assert(node_value.len_alloc > node_value.len_used); /* Ensure space for null terminator */
				node_value.buf_addr[node_value.len_used] = '\0';    /* Null terminate string */
				is_view = (0 == strcmp(node_value.buf_addr, OCTOLIT_VIEW)) ? TRUE : FALSE;
				break;
			case YDB_ERR_GVUNDEF:
				// No nodes of the given name exist
				node_found = FALSE;
				break;
			default:
				YDB_ERROR_CHECK(status);
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
				break;
			}
			if (!node_found) {
				/* View to be dropped does not exist */
				if (FALSE == drop_view->if_exists_specified) {
					// ERR_CANNOT_DROP_VIEW if no view exists and IF EXISTS not specified
					ERROR(ERR_CANNOT_DROP_VIEW, viewname);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				} else {
					// INFO
					INFO(INFO_VIEW_DOES_NOT_EXIST, viewname);
					PRINT_COMMAND_TAG(DROP_VIEW_COMMAND_TAG);
					CLEANUP_AND_RETURN_WITH_SUCCESS(memory_chunks, buffer, spcfc_buffer, query_lock,
									&cursor_ydb_buff);
				}
			} else if (!config->is_auto_upgrade_octo929) {
				/* Check if another view relies on this view. If so, disallow the DROP VIEW.
				 * We don't want to issue this error in case of the YDBOcto#929 auto upgrade (where it is
				 * possible for such scenarios to arise but it is okay because we are going to recreate all
				 * tables/functions/views and so all dependencies are going to be recomputed. Hence the
				 * "else if (!config->is_auto_upgrade_octo929)" check above.
				 */
				if (!is_view) {
					ERROR(ERR_WRONG_TYPE, viewname, OCTOLIT_VIEW);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				}
				/* When a view relies on this view, we should see gvn nodes of the following form
				 * exist in the database.
				 *	^%ydboctoocto("viewdependency","V1","fromview","V2")=""
				 * where
				 *	"V1" = view name being dropped
				 *	"V2" = view name which depends on this view
				 * Therefore check if ^%ydboctoocto("viewdependency","V1","fromview",*)
				 * nodes exist and if so issue an error.
				 */
				// gvn_subs[0] -> ^%ydboctoocto
				// gvn_subs[1] -> "viewdependency"
				// gvn_subs[2] -> view_name
				// gvn_subs[3] -> "fromview"
				ydb_buffer_t gvn_subs[6];
				YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
				YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWDEPENDENCY, &gvn_subs[1]);
				YDB_STRING_TO_BUFFER(viewname, &gvn_subs[2]);
				YDB_LITERAL_TO_BUFFER(OCTOLIT_FROMVIEW, &gvn_subs[3]);
				char view_name_buffer[OCTO_MAX_IDENT + 1];
				gvn_subs[4].buf_addr = view_name_buffer;
				gvn_subs[4].len_alloc = sizeof(view_name_buffer) - 1; /* reserve 1 byte for null terminator */
				gvn_subs[4].len_used = 0;
				status = ydb_subscript_next_s(&gvn_subs[0], 4, &gvn_subs[1], &gvn_subs[4]);
				if (YDB_ERR_NODEEND != status) {
					/* There is atleast one view which relies on this view. Issue error */
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					}
					gvn_subs[4].buf_addr[gvn_subs[4].len_used] = '\0'; /* null terminate view name */
					ERROR(ERR_DROP_VIEW_DEPENDS_ON_VIEW, viewname, gvn_subs[4].buf_addr);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				}
			}
			YDB_STRING_TO_BUFFER(viewname, view_name_buffer);
			/* Check if OIDs were created for this view.
			 * If so, delete those nodes from the catalog now that this view is going away.
			 */
			status = delete_table_or_view_from_pg_class(view_name_buffer);
			if (0 != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			// No viewgvname population required as a view doesn't have a ^view global
			// Empty string is good enough parameter to indicate that this is Drop statement call
			ydb_buffer_t gvname_buff;
			YDB_STRING_TO_BUFFER("", &gvname_buff);

			/* discard all plans and dependency global nodes associated with the view being dropped.
			 */
			ci_param1.address = view_name_buffer->buf_addr;
			ci_param1.length = view_name_buffer->len_used;
			ci_param2.address = gvname_buff.buf_addr;
			ci_param2.length = gvname_buff.len_used;
			status = ydb_ci("_ydboctoDiscardView", &ci_param1, &ci_param2);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);

			/* Now that OIDs and plan nodes have been clean up, dropping the view is a simple
			 * KILL ^%ydboctoschema(VIEWNAME)
			 */
			status = ydb_delete_s(&schema_global, 1, view_name_buffer, YDB_DEL_TREE);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);

			/* View is stored in memory chunks which are free'd at the end of query execution,
			 * so no additional memory de-allocation required.
			 */
		} else {
			// CREATE VIEW case. More processing needed.
			assert(create_view_STATEMENT == result_type);

			// Check if view or table of the same name already exists
			UNPACK_SQL_STATEMENT(view, result, create_view);
			UNPACK_SQL_STATEMENT(value, view->viewName, value);
			viewname = value->v.reference;
			SqlStatement *sql_view_or_table_stmt = find_view_or_table(viewname);
			if (NULL != sql_view_or_table_stmt) {
				// View already exists.
				ERROR(ERR_CANNOT_CREATE_VIEW, viewname);
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			out = open_memstream(&buffer, &buffer_size);
			assert(out);

			// Emit text definition
			int text_view_defn_length = 0;
			text_view_defn_length
			    += fprintf(out, "%.*s", cur_input_index - old_input_index, input_buffer_combined + old_input_index);
			fclose(out); // at this point "buffer" and "buffer_size" are usable
			if (0 > text_view_defn_length) {
				// Cleanup the buffer and exit with error status
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			// Print the converted text representation of the CREATE VIEW command
			INFO(INFO_TEXT_REPRESENTATION, buffer);
			/* Store the text representation of the CREATE VIEW statement:
			 *	^%ydboctoschema(view_name,OCTOLIT_TEXT)
			 */
			YDB_STRING_TO_BUFFER(viewname, view_name_buffer);
			status = store_table_definition(view_name_buffer, buffer, text_view_defn_length, TRUE);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			free(buffer);
			buffer = NULL; /* So CLEANUP_AND_RETURN_WITH_ERROR* macro calls below do not try "free(buffer)" */
			/* First store view name in catalog. As we need that OID to store in the binary view definition */
			status = store_table_or_view_in_pg_class(result, view_name_buffer);
			/* Cannot use CLEANUP_AND_RETURN_IF_NOT_YDB_OK macro here because the above function could set
			 * status to 1 to indicate an error (not necessarily a valid YDB_ERR_* code). In case it is a
			 * YDB error code, the YDB_ERROR_CHECK call would have already been done inside "store_table_in_pg_class"
			 * so all we need to do here is check if status is not 0 (aka YDB_OK) and if so invoke
			 * CLEANUP_AND_RETURN_WITH_ERROR.
			 */
			if (YDB_OK != status) {
				CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
			}
			compress_statement(result, &spcfc_buffer, &length, TRUE); /* Sets "spcfc_buffer" to "malloc"ed storage */
			assert(NULL != spcfc_buffer);
			status = store_table_definition(view_name_buffers, spcfc_buffer, length, FALSE);
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
		if (drop_view_STATEMENT == result_type) {
			PRINT_COMMAND_TAG(DROP_VIEW_COMMAND_TAG);
		} else {
			PRINT_COMMAND_TAG(CREATE_VIEW_COMMAND_TAG);
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
			drop_function = NULL; // Included to prevent Ninja [-Wmaybe-uninitialized] compiler warning:
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
		generate_name_type(FunctionHash, &state, 0, function_hash, sizeof(function_hash));
		function_hash_buffer = &function_name_buffers[2];
		YDB_STRING_TO_BUFFER(function_hash, function_hash_buffer);
		if (config->is_auto_upgrade_octo929) {
			/* Note down that a CREATE FUNCTION or DROP FUNCTION of this function/parameter-list happened.
			 * Later used to avoid duplicate CREATE FUNCTION/DROP FUNCTION calls during the auto upgrade process.
			 */
			ydb_buffer_t ydbocto929, func_subs[2];
			char	     subs0_buff[INT32_TO_STRING_MAX];

			YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTO929, &ydbocto929);
			func_subs[0].buf_addr = subs0_buff;
			func_subs[0].len_alloc = sizeof(subs0_buff);
			func_subs[0].len_used = snprintf(func_subs[0].buf_addr, func_subs[0].len_alloc, "%d", result_type);
			func_subs[1] = *function_hash_buffer;
			status = ydb_set_s(&ydbocto929, 2, &func_subs[0], NULL);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		}
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
		// - 1: don't include null terminator in size calculations
		function_name_buffers[4].len_alloc = sizeof(filename) - 1;

		/* Check if function with computed hash exists already or not */
		status = ydb_data_s(&octo_global, 3, &function_name_buffers[0], &ret_value);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, memory_chunks, buffer, spcfc_buffer, query_lock, &cursor_ydb_buff);
		if (drop_function_STATEMENT == result_type) {
			/* DROP FUNCTION */
			assert(NULL != drop_function);
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
					PRINT_COMMAND_TAG(DROP_FUNCTION_COMMAND_TAG);
					CLEANUP_AND_RETURN_WITH_SUCCESS(memory_chunks, buffer, spcfc_buffer, query_lock,
									&cursor_ydb_buff);
				}
			} else if (!config->is_auto_upgrade_octo929) {
				/* Check if a table CHECK constraint or EXTRACT column or a VIEW relies on this function.
				 * If so, disallow the DROP FUNCTION. In that case, gvn nodes of the following form would
				 * exist in the database:
				 *	^%ydboctoocto("functions","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G",
				 *						"check_constraint","NAMES","NAME1")=""
				 * OR
				 *	^%ydboctoocto("functions","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G",
				 *						"extract_function","NAMES","NAME1")=""
				 * where
				 *	"SAMEVALUE" = User visible SQL function name
				 *	"%ydboctoFN0uUSDY6E7G9VcjaOGNP9G" = Function hash (includes input/result parameter types)
				 *	"NAMES" = Table Name
				 *	"NAME1" = CHECK Constraint or EXTRACT Column name
				 *
				 * Therefore check if ^%ydboctoocto("functions",function_name,function_hash,"check_constraint",*)
				 * nodes exist and if so issue an error.
				 *
				 * But we don't want to issue this error in case of the YDBOcto#929 auto upgrade (where it is
				 * possible for such scenarios to arise but it is okay because we are going to recreate all
				 * tables/functions/views and so all dependencies are going to be recomputed. Hence the
				 * "else if (!config->is_auto_upgrade_octo929)" check above.
				 */
				ydb_buffer_t gvn_subs[7];
				YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
				YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &gvn_subs[1]);
				gvn_subs[2] = *function_name_buffer;
				gvn_subs[3] = *function_hash_buffer;
				YDB_LITERAL_TO_BUFFER(OCTOLIT_CHECK_CONSTRAINT, &gvn_subs[4]);

				char table_name_buff[OCTO_MAX_IDENT + 1];
				gvn_subs[5].buf_addr = table_name_buff;
				gvn_subs[5].len_alloc = sizeof(table_name_buff) - 1; /* reserve 1 byte for null terminator */

				char constraint_name_buff[OCTO_MAX_IDENT + 1];
				gvn_subs[6].buf_addr = constraint_name_buff;
				gvn_subs[6].len_alloc = sizeof(constraint_name_buff) - 1; /* reserve 1 byte for null terminator */

				// Check for CHECK constraints dependent on this function
				gvn_subs[5].len_used = 0;
				status = ydb_subscript_next_s(&gvn_subs[0], 5, &gvn_subs[1], &gvn_subs[5]);
				if (YDB_ERR_NODEEND != status) {
					/* There is at least one constraint which relies on this function. Issue error. */
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					}
					gvn_subs[6].len_used = 0;
					status = ydb_subscript_next_s(&gvn_subs[0], 6, &gvn_subs[1], &gvn_subs[6]);

					char fn_name[1024];
					get_function_name_and_parmtypes(
					    fn_name, sizeof(fn_name), function_name,
					    ((NULL == drop_function) ? NULL : drop_function->parameter_type_list));
					gvn_subs[5].buf_addr[gvn_subs[5].len_used] = '\0'; /* null terminate table name */
					gvn_subs[6].buf_addr[gvn_subs[6].len_used] = '\0'; /* null terminate constraint name */
					ERROR(ERR_DROP_FUNCTION_DEPENDS, fn_name, OCTOLIT_CONSTRAINT, gvn_subs[6].buf_addr,
					      gvn_subs[5].buf_addr);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				}
				// Check for EXTRACT columns dependent on this function
				YDB_LITERAL_TO_BUFFER(OCTOLIT_EXTRACTFUNCTION, &gvn_subs[4]);
				gvn_subs[5].len_used = 0;
				status = ydb_subscript_next_s(&gvn_subs[0], 5, &gvn_subs[1], &gvn_subs[5]);
				if (YDB_ERR_NODEEND != status) {
					/* There is at least one EXTRACT column which relies on this function. Issue error. */
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					}
					gvn_subs[6].len_used = 0;
					status = ydb_subscript_next_s(&gvn_subs[0], 6, &gvn_subs[1], &gvn_subs[6]);

					char fn_name[1024];
					get_function_name_and_parmtypes(
					    fn_name, sizeof(fn_name), function_name,
					    ((NULL == drop_function) ? NULL : drop_function->parameter_type_list));
					gvn_subs[5].buf_addr[gvn_subs[5].len_used] = '\0'; /* null terminate table name */
					gvn_subs[6].buf_addr[gvn_subs[6].len_used] = '\0'; /* null terminate constraint name */
					ERROR(ERR_DROP_FUNCTION_DEPENDS, fn_name, OCTOLIT_COLUMN, gvn_subs[6].buf_addr,
					      gvn_subs[5].buf_addr);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
								      &cursor_ydb_buff);
				}
				/* Check if a View relies on this function. If so, disallow the DROP FUNCTION.
				 * If a View relies on this function, we would see gvn nodes of the following form
				 * exist in the database.
				 *	^%ydboctoocto("functionviewdependency","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G",
				 *						"V1")=""
				 * where
				 *	"SAMEVALUE" = User visible SQL function name
				 *	"%ydboctoFN0uUSDY6E7G9VcjaOGNP9G" = Function hash (includes input/result parameter types)
				 *	"V1" = View Name
				 *
				 * Therefore check if ^%ydboctoocto("functions",function_name,function_hash,"view_dependency",*)
				 * nodes exist and if so issue an error.
				 */
				// gvn_subs[0] -> ^%ydboctoocto
				// gvn_subs[1] -> "functionviewdependency"
				// gvn_subs[2] -> function_name
				// gvn_subs[3] -> function_hash
				YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONVIEWDEPENDENCY, &gvn_subs[1]);

				char view_name_buffer[OCTO_MAX_IDENT + 1];
				gvn_subs[4].buf_addr = view_name_buffer;
				gvn_subs[4].len_alloc = sizeof(view_name_buffer) - 1; /* reserve 1 byte for null terminator */
				gvn_subs[4].len_used = 0;
				status = ydb_subscript_next_s(&gvn_subs[0], 4, &gvn_subs[1], &gvn_subs[4]);
				if (YDB_ERR_NODEEND != status) {
					/* There is at least one view which relies on this function. Issue error */
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
									      &cursor_ydb_buff);
					}
					char fn_name[1024];
					get_function_name_and_parmtypes(fn_name, sizeof(fn_name), function_name,
									drop_function->parameter_type_list);
					gvn_subs[4].buf_addr[gvn_subs[4].len_used] = '\0'; /* null terminate view name */
					ERROR(ERR_DROP_FUNCTION_DEPENDS_ON_VIEW, fn_name, gvn_subs[4].buf_addr);
					CLEANUP_AND_RETURN_WITH_ERROR(memory_chunks, buffer, spcfc_buffer, query_lock,
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
						PRINT_COMMAND_TAG(CREATE_FUNCTION_COMMAND_TAG);
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

			ydb_int_t auto_load;
			/* The M function we are about to invoke has special code in case "auto_load" parameter is non-zero.
			 * It skips deleting dependencies as it is not sure if CREATE TABLE commands for tables/views that
			 * depend on this function would be rerun. But in case of the YDBOcto#929 auto upgrade, all existing
			 * tables/functions/views are going to be auto upgraded unconditionally. Therefore, it is safe to
			 * pass a zero "auto_load" parameter in that case.
			 */
			auto_load = config->is_auto_upgrade_octo929 ? FALSE : config->in_auto_load_octo_seed;
			status = ydb_ci("_ydboctoDiscardFunction", &ci_param1, &ci_param2, auto_load);
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

			/* First store function name in catalog. As we need that OID to store in the binary function
			 * definition. The below call also sets function->oid which is needed before the call to
			 * "compress_statement" as that way the oid also gets stored in the binary function definition.
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
			compress_statement(result, &spcfc_buffer, &length, FALSE); /* Sets "spcfc_buffer" to "malloc"ed storage */
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
		if (drop_function_STATEMENT == result_type) {
			PRINT_COMMAND_TAG(DROP_FUNCTION_COMMAND_TAG);
		} else {
			PRINT_COMMAND_TAG(CREATE_FUNCTION_COMMAND_TAG);
		}
		release_query_lock = FALSE; /* Set variable to FALSE so we do not try releasing same lock later */
		break;
	case begin_STATEMENT:
	case commit_STATEMENT:
		ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "transactions");
		cursor_used = FALSE; /* Remove this line once this feature gets implemented */
		break;
	case dynamic_sql_STATEMENT:
		/* Do not issue a ERR_FEATURE_NOT_IMPLEMENTED warning. Just silently do nothing. (YDBOcto#958) */
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
	/* Log the INFO_EXECUTION_DONE message in case for Octo and for the Simple Query protocol in case of rocto.
	 * In the Extended Query Protocol case of rocto, we are currently handling just the Parse message. We still
	 * need to handle the Bind, Describe and Execute messages. Since the result rows will not be sent until the
	 * Execute message is handled, we need to delay logging the INFO_EXECUTION_DONE message until then. This
	 * will be logged later in "handle_execute.c". Hence the "is_extended_query" check below.
	 */
	if (!parse_context->is_extended_query) {
		INFO(INFO_EXECUTION_DONE, "");
	}
	return 0;
}
