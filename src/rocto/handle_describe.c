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
	ydb_buffer_t	      routine_buf, filename_buf;
	ydb_buffer_t	      describe_subs[5];
	uint32_t	      found = 0;
	int32_t		      status;
	char		      filename[OCTO_PATH_MAX];
	char		      routine_str[MAX_ROUTINE_LEN + 1]; // Null terminator

	// Fetch the named SQL query from the session, either OCTOLIT_PREPARED or OCTOLIT_BOUND depending on Describe message type:
	// ^session(id, OCTOLIT_PREPARED, <name>) or ^session(id, OCTOLIT_BOUND, <name>)
	YDB_STRING_TO_BUFFER(config->global_names.session, &describe_subs[0])
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &describe_subs[1])
	YDB_STRING_TO_BUFFER(describe->name, &describe_subs[3])
	YDB_STRING_TO_BUFFER(OCTOLIT_ROUTINE, &describe_subs[4])
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

	// Retrieve routine name
	OCTO_SET_BUFFER(routine_buf, routine_str);
	status = ydb_get_s(&describe_subs[0], 4, &describe_subs[1], &routine_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	routine_buf.buf_addr[routine_buf.len_used] = '\0';

	status = strncmp("none", routine_buf.buf_addr, MAX_ROUTINE_LEN);
	if (0 == status) {
		no_data = make_no_data();
		send_message(session, (BaseMessage *)(&no_data->type));
		free(no_data);
	} else {
		/* The below call updates "filename" to be the full path including "routine_name" at the end */
		status = get_full_path_of_generated_m_file(filename, sizeof(filename), &routine_buf.buf_addr[1]);
		if (status) {
			/* Error message would have already been issued in above function call. Just return non-zero status. */
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

	return 0;
}
