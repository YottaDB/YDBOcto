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
	ParameterDescription	*parm_description;
	RowDescription		*description;
	NoData			*no_data;
	ydb_buffer_t		routine_buf, filename_buf;
	ydb_buffer_t		*describe_subs;
	uint32_t		found = 0;
	int32_t			done = FALSE, status, routine_len = MAX_ROUTINE_LEN;
	char			filename[OCTO_PATH_MAX];

	// Fetch the named SQL query from the session, either "prepared" or "bound" depending on Describe message type:
	// ^session(id, "prepared", <name>) or ^session(id, "bound", <name>)
	if ('S' == describe->item) {
		// Client is seeking a ParameterDescription, make and send one
		LOG_LOCAL_ONLY(TRACE, INFO_ROCTO_PARAMETER_DESCRIPTION_SENT, describe->name);
		parm_description = make_parameter_description(describe->name, session);
		if (NULL == parm_description)
			return 1;
		send_message(session, (BaseMessage*)(&parm_description->type));
		free(parm_description);
		describe_subs = make_buffers(config->global_names.session, 4, session->session_id->buf_addr, "prepared", describe->name, "routine");
	} else {
		describe_subs = make_buffers(config->global_names.session, 4, session->session_id->buf_addr, "bound", describe->name, "routine");
	}

	// Check if portal exists
	status = ydb_data_s(&describe_subs[0], 3, &describe_subs[1], &found);
	YDB_ERROR_CHECK(status);
	if ((YDB_OK != status) || (0 == found)) {
		free(describe_subs);
		return 1;
	}

	// Retrieve routine name
	YDB_MALLOC_BUFFER(&routine_buf, MAX_ROUTINE_LEN + 1);		// Null terminator
	status = ydb_get_s(&describe_subs[0], 4, &describe_subs[1], &routine_buf);
	free(describe_subs);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&routine_buf);
		return 1;
	}
	routine_buf.buf_addr[routine_buf.len_used] = '\0';

	status = strncmp("none", routine_buf.buf_addr, routine_len);
	if (0 == status) {
		no_data = make_no_data();
		send_message(session, (BaseMessage*)(&no_data->type));
		free(no_data);
	} else {
		GET_FULL_PATH_OF_GENERATED_M_FILE(filename, &routine_buf.buf_addr[1]);	/* updates "filename" to be full path */
		YDB_MALLOC_BUFFER(&filename_buf, OCTO_PATH_MAX);
		YDB_COPY_STRING_TO_BUFFER(filename, &filename_buf, done);
		if (!done) {
			ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
			YDB_FREE_BUFFER(&filename_buf);
			YDB_FREE_BUFFER(&routine_buf);
			return 1;
		}
		description = get_plan_row_description(&filename_buf);
		send_message(session, (BaseMessage*)(&description->type));
		if ('S' == describe->item) {
			TRACE(INFO_ROCTO_ROW_DESCRIPTION_SENT, "prepared statement", describe->name);
		} else {
			TRACE(INFO_ROCTO_ROW_DESCRIPTION_SENT, "portal", describe->name);
		}
		YDB_FREE_BUFFER(&filename_buf);
		free(description);
	}
	YDB_FREE_BUFFER(&routine_buf);

	return 0;
}
