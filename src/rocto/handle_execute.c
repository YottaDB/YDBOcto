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

int handle_execute(Execute *execute, RoctoSession *session) {
	CommandComplete		*response;
	QueryResponseParms	parms;
	EmptyQueryResponse	*empty;
	SqlStatement		stmt;
	ydb_buffer_t		*portal_subs, *cursor_subs;
	ydb_buffer_t		routine_buffer, tag_buf, num_parms_buf, parm_buf;
	ydb_buffer_t		schema_global, cursor_buffer;
	ydb_string_t		ci_filename, ci_routine;
	ydb_long_t		result, cursorId;
	boolean_t		canceled = FALSE;
	long int		temp_long;
	int32_t			status, done;
	int16_t			num_parms, cur_parm, cur_parm_temp;
	char			filename[OCTO_PATH_MAX];
	SqlStatementType	command_tag;

	TRACE(ERR_ENTERING_FUNCTION, "handle_execute");

	// Set subscripts to access routine on portal: ^session(id, "bound", <name>, "routine"
	portal_subs = make_buffers(config->global_names.session, 4, session->session_id->buf_addr, "bound", execute->source, "routine");
	// Retrieve routine from portal
	YDB_MALLOC_BUFFER(&routine_buffer, MAX_ROUTINE_LEN + 1);	// Null terminator
	status = ydb_get_s(&portal_subs[0], 4, &portal_subs[1], &routine_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&routine_buffer);
		return 1;
	}
	routine_buffer.buf_addr[routine_buffer.len_used] = '\0';

	// Retrieve command tag from portal
	YDB_MALLOC_BUFFER(&portal_subs[4], MAX_TAG_LEN);
	YDB_COPY_STRING_TO_BUFFER("tag", &portal_subs[4], done);
	if (!done) {
		YDB_FREE_BUFFER(&routine_buffer);
		YDB_FREE_BUFFER(&portal_subs[4]);
		free(portal_subs);
		return 1;
	}
	YDB_MALLOC_BUFFER(&tag_buf, INT32_TO_STRING_MAX);
	status = ydb_get_s(&portal_subs[0], 4, &portal_subs[1], &tag_buf);
	YDB_FREE_BUFFER(&portal_subs[4]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&tag_buf);
		YDB_FREE_BUFFER(&routine_buffer);
		free(portal_subs);
		return 1;
	}
	tag_buf.buf_addr[tag_buf.len_used] = '\0';
	temp_long = strtol(tag_buf.buf_addr, NULL, 10);
	YDB_FREE_BUFFER(&tag_buf);
	if ((ERANGE != errno) && (0 <= temp_long) && (INT32_MAX >= temp_long) && (invalid_STATEMENT >= temp_long)) {
		command_tag = (int32_t)temp_long;
	} else {
		YDB_FREE_BUFFER(&routine_buffer);
		free(portal_subs);
		return 1;
	}

	// Skip plan execution for SET and SHOW statements
	if (0 != strncmp(routine_buffer.buf_addr, "none", MAX_ROUTINE_LEN)) {
		// Fetch number of parameters
		free(portal_subs);
		portal_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, "bound", execute->source,
				"parameters", "all", "");
		YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
		status = ydb_get_s(&portal_subs[0], 5, &portal_subs[1], &num_parms_buf);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&routine_buffer);
			YDB_FREE_BUFFER(&num_parms_buf);
			free(portal_subs);
			return 1;
		}
		num_parms_buf.buf_addr[num_parms_buf.len_used] = '\0';
		result = strtol(num_parms_buf.buf_addr, NULL, 10);
		YDB_FREE_BUFFER(&num_parms_buf);
		if ((ERANGE != errno) && (0 <= result) && (INT16_MAX >= result)) {
			num_parms = (int16_t)result;
		} else {
			ERROR(ERR_LIBCALL, "strtol")
			YDB_FREE_BUFFER(&routine_buffer);
			free(portal_subs);
			return 1;
		}

		// Create a new cursor
		YDB_MALLOC_BUFFER(&cursor_buffer, MAX_STR_CONST);
		YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
		cursorId = create_cursor(&schema_global, &cursor_buffer);

		cursor_subs = make_buffers(config->global_names.cursor, 3, cursor_buffer.buf_addr, "parameters", "");
		YDB_MALLOC_BUFFER(&cursor_subs[3], INT16_TO_STRING_MAX);
		YDB_MALLOC_BUFFER(&portal_subs[6], INT16_TO_STRING_MAX);
		YDB_MALLOC_BUFFER(&parm_buf, MAX_STR_CONST);
		for (cur_parm = 0; cur_parm < num_parms; cur_parm++) {
			// Get parameter value
			cur_parm_temp = cur_parm + 1;
			OCTO_INT16_TO_BUFFER(cur_parm_temp, &portal_subs[6]);	// Convert from 0-indexed to 1-indexed
			status = ydb_get_s(&portal_subs[0], 6, &portal_subs[1], &parm_buf);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				// Only cleanup tree after one or more parameters is set
				if (0 < cur_parm) {
					status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
					YDB_ERROR_CHECK(status);
				}
				YDB_FREE_BUFFER(&portal_subs[6]);
				YDB_FREE_BUFFER(&cursor_subs[3]);
				YDB_FREE_BUFFER(&parm_buf);
				free(portal_subs);
				free(cursor_subs);
				return 1;
			}
			parm_buf.buf_addr[parm_buf.len_used] = '\0';
			// Store parameter value on cursor
			OCTO_INT16_TO_BUFFER(cur_parm_temp, &cursor_subs[3]);	// Convert from 0-indexed to 1-indexed
			status = ydb_set_s(&cursor_subs[0], 3, &cursor_subs[1], &parm_buf);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				// Only cleanup tree after one or more parameters is set
				if (0 < cur_parm) {
					status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
					YDB_ERROR_CHECK(status);
				}
				YDB_FREE_BUFFER(&portal_subs[6]);
				YDB_FREE_BUFFER(&cursor_subs[3]);
				YDB_FREE_BUFFER(&parm_buf);
				free(portal_subs);
				free(cursor_subs);
				return 1;
			}
		}
		YDB_FREE_BUFFER(&portal_subs[6]);
		YDB_FREE_BUFFER(&parm_buf);
		free(portal_subs);

		// Prepare callback parameters and call-in interface to execute query
		memset(&parms, 0, sizeof(QueryResponseParms));
		parms.session = session;
		parms.max_data_to_send = execute->rows_to_return;
		stmt.type = select_STATEMENT;		// Only need this for the type to let the callee know we are running a SELECT statement
		GET_FULL_PATH_OF_GENERATED_M_FILE(filename, &routine_buffer.buf_addr[1]);	/* updates "filename" to be full path */
		ci_filename.address = filename;
		ci_filename.length = strlen(filename);
		ci_routine.address = routine_buffer.buf_addr;
		ci_routine.length = routine_buffer.len_used;
		// Run the target routine
		// cursorId is typecast here since the YottaDB call-in interface does not yet support 64-bit parameters
		status = ydb_ci("_ydboctoselect", cursorId, &ci_filename, &ci_routine);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			// Cleanup cursor parameters
			status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
			YDB_FREE_BUFFER(&cursor_subs[3]);
			YDB_FREE_BUFFER(&cursor_buffer);
			YDB_FREE_BUFFER(&routine_buffer);
			free(cursor_subs);
			return 1;
		}
		// Check for cancel requests
		if (config->is_rocto) {
			canceled = is_query_canceled(&handle_query_response, cursorId, (void*)&parms, filename, TRUE);
			if (canceled) {
				// Cleanup cursor parameters
				status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
				YDB_ERROR_CHECK(status);
				YDB_FREE_BUFFER(&cursor_subs[3]);
				YDB_FREE_BUFFER(&cursor_buffer);
				YDB_FREE_BUFFER(&routine_buffer);
				free(cursor_subs);
				return -1;
			}
		}
		// Send back data rows, but pass FALSE to omit row descriptions as they are not expected in response to Execute messages
		status = handle_query_response(&stmt, cursorId, (void*)&parms, filename, FALSE);
		if (0 != status) {
			// Cleanup cursor parameters
			status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
			YDB_FREE_BUFFER(&cursor_subs[3]);
			YDB_FREE_BUFFER(&cursor_buffer);
			YDB_FREE_BUFFER(&routine_buffer);
			free(cursor_subs);
			return 1;
		}
		// Cleanup cursor parameters
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&cursor_subs[3]);
		YDB_FREE_BUFFER(&cursor_buffer);
		free(cursor_subs);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&routine_buffer);
			return 1;
		}
	} else {
		parms.data_sent = FALSE;
		free(portal_subs);
	}
	YDB_FREE_BUFFER(&routine_buffer);

	// TODO: we need to limit the returns and provide a PortalSuspend if a limit on rows was requested per issue #255
	// For now, we always return all rows

	response = make_command_complete(command_tag, parms.rows_sent);
	if (NULL != response) {
		send_message(session, (BaseMessage*)(&response->type));
		free(response);
	}
	if (!parms.data_sent) {
		empty = make_empty_query_response();
		send_message(session, (BaseMessage*)(&empty->type));
		free(empty);
	}

	return 0;
}
