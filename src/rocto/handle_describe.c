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
	ParameterDescription *parm_description;
	RowDescription *      description;
	NoData *	      no_data;
	ydb_buffer_t	      routine_buf, filename_buf, tag_buf;
	ydb_buffer_t	      describe_subs[5];
	uint32_t	      found = 0;
	int32_t		      status;
	char		      filename[OCTO_PATH_MAX];
	char		      routine_str[MAX_ROUTINE_LEN + 1]; // Null terminator
	char		      tag_str[INT32_TO_STRING_MAX];
	SqlStatementType      command_tag;
	long int	      temp_long;

	/* Fetch the named SQL query from the session, either OCTOLIT_PREPARED or OCTOLIT_BOUND depending on Describe message type:
	 * ^session(id, OCTOLIT_PREPARED, <name>)	--> If describe message is Prepared Statement variant
	 * ^session(id, OCTOLIT_BOUND, <name>)		--> If describe message is Portal variant
	 */
	YDB_STRING_TO_BUFFER(config->global_names.session, &describe_subs[0])
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &describe_subs[1])
	YDB_STRING_TO_BUFFER(describe->name, &describe_subs[3])
	if ('S' == describe->item) {
		// Client is seeking a ParameterDescription, make and send one
		LOG_LOCAL_ONLY(TRACE, INFO_ROCTO_PARAMETER_DESCRIPTION_SENT, describe->name);
		parm_description = make_parameter_description(describe->name, session);
		if (NULL == parm_description)
			return 1;
		send_message(session, (BaseMessage *)(&parm_description->type));
		free(parm_description);
		YDB_STRING_TO_BUFFER(OCTOLIT_PREPARED, &describe_subs[2])
	} else {
		YDB_STRING_TO_BUFFER(OCTOLIT_BOUND, &describe_subs[2])
	}

	// Check if portal exists
	status = ydb_data_s(&describe_subs[0], 3, &describe_subs[1], &found);
	YDB_ERROR_CHECK(status);
	if ((YDB_OK != status) || (0 == found)) {
		return 1;
	}

	/* Retrieve command tag to find out which type of operation SELECT, INSERT etc.
	 * If it is for example an INSERT operation, then there is no corresponding row description even though
	 * there is a corresponding routine (i.e. M plan) to execute. So send a NoData message in that case.
	 */
	YDB_STRING_TO_BUFFER(OCTOLIT_TAG, &describe_subs[4]);
	OCTO_SET_NULL_TERMINATED_BUFFER(tag_buf, tag_str);
	status = ydb_get_s(&describe_subs[0], 4, &describe_subs[1], &tag_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	tag_buf.buf_addr[tag_buf.len_used] = '\0';
	temp_long = strtol(tag_buf.buf_addr, NULL, 10);
	if ((ERANGE != errno) && (0 <= temp_long) && (INT32_MAX >= temp_long) && (invalid_STATEMENT >= temp_long)) {
		command_tag = (int32_t)temp_long;
	} else {
		return 1;
	}
	switch (command_tag) {
	case insert_STATEMENT:
	case set_STATEMENT:
		/* No row descriptions for INSERT or SET. Send a NoData message in that case. */
		no_data = make_no_data();
		send_message(session, (BaseMessage *)(&no_data->type));
		free(no_data);
		break;
	case show_STATEMENT: {
		SqlStatement	   stmt;
		QueryResponseParms parms;
		ydb_buffer_t	   parm_value_buf;

		YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLE, &describe_subs[4])
		OCTO_MALLOC_NULL_TERMINATED_BUFFER(&parm_value_buf, OCTO_INIT_BUFFER_LEN);
		status = ydb_get_s(&describe_subs[0], 4, &describe_subs[1], &parm_value_buf);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(parm_value_buf);
			status = ydb_get_s(&describe_subs[0], 4, &describe_subs[1], &parm_value_buf);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&parm_value_buf);
			return 1;
		}
		parm_value_buf.buf_addr[parm_value_buf.len_used] = '\0'; /* null-terminated buffer needed below */

		stmt.type = show_STATEMENT;
		memset(&parms, 0, sizeof(QueryResponseParms));
		parms.session = session;
		parms.parm_name = parm_value_buf.buf_addr;
		status = handle_query_response(&stmt, 0, (void *)&parms, NULL, PSQL_Describe);
		YDB_FREE_BUFFER(&parm_value_buf);
		if (YDB_OK != status) {
			return 1;
		}
		break;
	}
	default:
		// Retrieve routine name
		YDB_STRING_TO_BUFFER(OCTOLIT_ROUTINE, &describe_subs[4])
		OCTO_SET_NULL_TERMINATED_BUFFER(routine_buf, routine_str);
		status = ydb_get_s(&describe_subs[0], 4, &describe_subs[1], &routine_buf);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
		routine_buf.buf_addr[routine_buf.len_used] = '\0';
		status = strncmp(OCTOLIT_NONE, routine_buf.buf_addr, MAX_ROUTINE_LEN);
		if (0 == status) {
			no_data = make_no_data();
			send_message(session, (BaseMessage *)(&no_data->type));
			free(no_data);
		} else {
			/* The below call updates "filename" to be the full path including "routine_name" at the end */
			status = get_full_path_of_generated_m_file(filename, sizeof(filename), &routine_buf.buf_addr[1]);
			if (status) {
				/* Error message would have already been issued in above function call. Just return non-zero status.
				 */
				return 1;
			}
			YDB_STRING_TO_BUFFER(filename, &filename_buf);
			description = get_plan_row_description(&filename_buf);
			if (NULL != description) {
				send_message(session, (BaseMessage *)(&description->type));
				if ('S' == describe->item) {
					TRACE(INFO_ROCTO_ROW_DESCRIPTION_SENT, "prepared statement", describe->name);
				} else {
					TRACE(INFO_ROCTO_ROW_DESCRIPTION_SENT, "portal", describe->name);
				}
				free(description);
			} else {
				return 1;
			}
		}
		break;
	}
	return 0;
}
