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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"
#include "helpers.h"

int32_t handle_execute(Execute *execute, RoctoSession *session, ydb_long_t *cursorId) {
	CommandComplete	   *command_complete;
	PortalSuspended	   *portal_suspended;
	QueryResponseParms  parms;
	EmptyQueryResponse *empty;
	ydb_buffer_t	    portal_subs[8], cursor_subs[5];
	ydb_buffer_t	    routine_buffer, tag_buf, num_parms_buf, parm_buf;
	ydb_buffer_t	    schema_global, cursor_buffer;
	ydb_string_t	    ci_routine;
	ydb_long_t	    result;
	boolean_t	    canceled = FALSE;
	long int	    temp_long;
	int32_t		    status, tmp_status;
	int16_t		    num_parms, cur_parm, cur_parm_temp;
	char		    filename[OCTO_PATH_MAX], routine_str[MAX_ROUTINE_LEN + 1]; // Null terminator
	char		    tag_str[INT32_TO_STRING_MAX], num_parms_str[INT16_TO_STRING_MAX];
	char		    cur_parm_str[INT16_TO_STRING_MAX], cursor_str[YDB_MAX_IDENT + 1]; // Null terminator
	SqlStatementType    command_tag;
	SqlStatement	    stmt;

	TRACE(INFO_ENTERING_FUNCTION, "handle_execute");

	// Prepare callback parameters
	memset(&parms, 0, sizeof(QueryResponseParms));
	parms.session = session;
	parms.parm_name = execute->source;
	parms.max_data_to_send = execute->rows_to_return;

	// Set subscripts to access routine on portal: ^session(id, OCTOLIT_BOUND, <name>, OCTOLIT_ROUTINE
	YDB_STRING_TO_BUFFER(config->global_names.session, &portal_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &portal_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_BOUND, &portal_subs[2]);
	YDB_STRING_TO_BUFFER(execute->source, &portal_subs[3]);
	YDB_STRING_TO_BUFFER(OCTOLIT_ROUTINE, &portal_subs[4]);
	// Retrieve routine from portal
	OCTO_SET_BUFFER(routine_buffer, routine_str);
	status = ydb_get_s(&portal_subs[0], 4, &portal_subs[1], &routine_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	routine_buffer.buf_addr[routine_buffer.len_used] = '\0';
	/* The below call updates "filename" to be the full path including "routine_name" at the end */
	status = get_full_path_of_generated_m_file(filename, sizeof(filename), &routine_buffer.buf_addr[1]);
	if (status) {
		/* Error message would have already been issued in above function call. Just return non-zero status. */
		return 1;
	}
	// Retrieve command tag from portal
	YDB_LITERAL_TO_BUFFER(OCTOLIT_TAG, &portal_subs[4]);
	OCTO_SET_NULL_TERMINATED_BUFFER(tag_buf, tag_str);
	status = ydb_get_s(&portal_subs[0], 4, &portal_subs[1], &tag_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	tag_buf.buf_addr[tag_buf.len_used] = '\0';
	temp_long = strtol(tag_buf.buf_addr, NULL, 10);
	if (!STRTOL_VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (INT32_MAX >= temp_long)
	    && (invalid_STATEMENT >= temp_long)) {
		command_tag = (int32_t)temp_long;
	} else {
		return 1;
	}

	/* Skip plan execution
	 * a) for query types that don't have any plans (e.g. SET, SHOW, CREATE) OR
	 * b) when the portal is suspended (rows remain to return on an existing cursor).
	 */
	if ((0 != strncmp(routine_buffer.buf_addr, OCTOLIT_NONE, MAX_ROUTINE_LEN)) && (0 > *cursorId)) {
		boolean_t wrapInTp;

		assert((select_STATEMENT == command_tag) || (insert_STATEMENT == command_tag)
		       || (delete_from_STATEMENT == command_tag) || (update_STATEMENT == command_tag));
		// Fetch number of parameters
		YDB_STRING_TO_BUFFER(OCTOLIT_PARAMETERS, &portal_subs[4]);
		YDB_STRING_TO_BUFFER(OCTOLIT_ALL, &portal_subs[5]);
		OCTO_SET_BUFFER(num_parms_buf, num_parms_str);
		status = ydb_get_s(&portal_subs[0], 5, &portal_subs[1], &num_parms_buf);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
		num_parms_buf.buf_addr[num_parms_buf.len_used] = '\0';
		result = strtol(num_parms_buf.buf_addr, NULL, 10);
		if (!STRTOL_VALUE_OUT_OF_RANGE(result) && (0 <= result) && (INT16_MAX >= result)) {
			num_parms = (int16_t)result;
		} else {
			ERROR(ERR_LIBCALL, "strtol")
			return 1;
		}

		// Create a new cursor
		OCTO_SET_BUFFER(cursor_buffer, cursor_str);
		YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
		*cursorId = create_cursor(&schema_global, &cursor_buffer);

		YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_subs[0]);
		YDB_STRING_TO_BUFFER(cursor_buffer.buf_addr, &cursor_subs[1]);
		YDB_STRING_TO_BUFFER(OCTOLIT_PARAMETERS, &cursor_subs[2]);
		YDB_STRING_TO_BUFFER(OCTOLIT_PARAMETERS, &portal_subs[4]);
		YDB_STRING_TO_BUFFER(OCTOLIT_ALL, &portal_subs[5]);
		OCTO_SET_BUFFER(cursor_subs[3], cur_parm_str);
		OCTO_SET_BUFFER(portal_subs[6], cur_parm_str);
		OCTO_MALLOC_NULL_TERMINATED_BUFFER(&parm_buf, OCTO_INIT_BUFFER_LEN);
		for (cur_parm = 0; cur_parm < num_parms; cur_parm++) {
			// Get parameter value
			cur_parm_temp = cur_parm + 1;
			// This takes care of cursor_subs[3] as well as these buffers share the same buf_addr
			OCTO_INT16_TO_BUFFER(cur_parm_temp, &portal_subs[6]); // Convert from 0-indexed to 1-indexed
			status = ydb_get_s(&portal_subs[0], 6, &portal_subs[1], &parm_buf);
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(parm_buf);
				status = ydb_get_s(&portal_subs[0], 6, &portal_subs[1], &parm_buf);
				assert(YDB_ERR_INVSTRLEN != status);
			}
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				// Only cleanup tree after one or more parameters is set
				if (0 < cur_parm) {
					status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
					YDB_ERROR_CHECK(status);
				}
				YDB_FREE_BUFFER(&parm_buf);
				return 1;
			}
			parm_buf.buf_addr[parm_buf.len_used] = '\0';
			// Store parameter value on cursor
			OCTO_INT16_TO_BUFFER(cur_parm_temp, &cursor_subs[3]); // Convert from 0-indexed to 1-indexed
			status = ydb_set_s(&cursor_subs[0], 3, &cursor_subs[1], &parm_buf);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				// Only cleanup tree after one or more parameters is set
				if (0 < cur_parm) {
					status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
					YDB_ERROR_CHECK(status);
				}
				YDB_FREE_BUFFER(&parm_buf);
				return 1;
			}
		}
		/* Check if Execute message has specified a non-zero max_data_to_send. If so, transfer this to the LIMIT
		 * keyword that was added to the SELECT query at Parse message time (in "src/parser.y") in case the LIMIT
		 * parameter is GREATER than the max_data_to_send value. That way, we only return the number of rows
		 * requested and not waste time computing rows that the user does not anyways want.
		 * Note that the LIMIT keyword addition would have happened only for SELECT queries and not for other
		 * commands like INSERT/DELETE/UPDATE hence the "select_STATEMENT == command_tag" check below.
		 */
		if ((select_STATEMENT == command_tag) && (parms.max_data_to_send)) {
			temp_long = strtol(parm_buf.buf_addr, NULL, 10);
			if (temp_long > parms.max_data_to_send) {
				parm_buf.len_used = snprintf(parm_buf.buf_addr, parm_buf.len_alloc, "%d", parms.max_data_to_send);
				assert(parm_buf.len_used <= parm_buf.len_alloc);
				status = ydb_set_s(&cursor_subs[0], 3, &cursor_subs[1], &parm_buf);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					YDB_FREE_BUFFER(&parm_buf);
					return 1;
				}
			}
		}
		YDB_FREE_BUFFER(&parm_buf);
		// Prepare call-in interface to execute query
		ci_routine.address = routine_buffer.buf_addr;
		ci_routine.length = routine_buffer.len_used;
		/* Currently read-only queries are not wrapped in TP and read-write queries are wrapped in TP. */
		wrapInTp = (select_STATEMENT != command_tag);
		// Run the target routine
		status = ydb_ci("_ydboctoselect", *cursorId, &ci_routine, (ydb_int_t)wrapInTp);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			// Cleanup cursor parameters
			status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
			return 1;
		}
		// Check for cancel requests
		if (config->is_rocto) {
			canceled = is_query_canceled(&handle_query_response);
			if (canceled) {
				// Cleanup cursor parameters
				status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
				YDB_ERROR_CHECK(status);
				return -1;
			}
		}
		stmt.type = command_tag; /* needed by "handle_query_response" to know which message to handle */
		status = handle_query_response(&stmt, *cursorId, (void *)&parms, filename, PSQL_Execute);
		tmp_status = status; // Store status so it doesn't get overwritten by result of ydb_delete_s
		/* Don't need to retain the cursor for later Execute messages if it is not the PortalSuspended case */
		if (PORTAL_SUSPENDED != status) {
			// Cleanup cursor parameters
			status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status)
				return 1;
			else
				status = tmp_status;
		}
	} else if (0 <= *cursorId) {
		// There is an active cursor and a PortalSuspended message was previously issued,
		// So rows remain to be sent
		status = send_result_rows(*cursorId, &parms, filename);
	} else {
		switch (command_tag) {
		case show_STATEMENT:
		case set_STATEMENT:
			stmt.type = command_tag;
			status = handle_query_response(&stmt, *cursorId, (void *)&parms, filename, PSQL_Execute);
			parms.data_sent = TRUE; /* to avoid sending EmptyQueryResponse message */
			break;
		default:
			parms.data_sent = FALSE;
			break;
		}
	}

	if (0 == status) { // No errors, all rows sent
		/* We are done sending all rows but we did not go through "run_query()" (because this is an extended query
		 * and we went through only the early part of "run_query()" for the Parse message, but not for the Bind,
		 * Describe and Execute messages) and so we did not log the INFO_EXECUTION_DONE message so do that now.
		 */
		LOG_LOCAL_ONLY(INFO, INFO_EXECUTION_DONE, "");
		command_complete = make_command_complete(command_tag, parms.row_count);
		if (NULL != command_complete) {
			send_message(session, (BaseMessage *)(&command_complete->type));
			free(command_complete);
		}
		*cursorId = -1;			 // Reset the cursor
	} else if (PORTAL_SUSPENDED == status) { // Not all rows sent, save cursor and issue PortalSuspended
		portal_suspended = make_portal_suspended();
		assert(NULL != portal_suspended);
		send_message(session, (BaseMessage *)(&portal_suspended->type));
		free(portal_suspended);
	} else {		// Error occurred
		*cursorId = -1; // Reset the cursor
		return 1;
	}
	if (!parms.data_sent) {
		empty = make_empty_query_response();
		send_message(session, (BaseMessage *)(&empty->type));
		free(empty);
	}
	return 0;
}
