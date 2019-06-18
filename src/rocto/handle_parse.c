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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "helpers.h"
#include "rocto.h"

int handle_parse(Parse *parse, RoctoSession *session) {
	// At the moment, we don't have "bound function"
	// This feature should be implemented before 1.0
	// For now, just a search-and-replace of anything starting with a '$'
	// This is not super great because it means one could have a SQLI attack
	int status;
	ydb_buffer_t *src_subs;
	ydb_buffer_t sql_expression, z_status, z_status_value;
	ParseComplete *response;

	TRACE(ERR_ENTERING_FUNCTION, "handle_parse");

	// zstatus buffers
	YDB_LITERAL_TO_BUFFER("$ZSTATUS", &z_status);
	YDB_MALLOC_BUFFER(&z_status_value, MAX_STR_CONST);

	// Fetch the named SQL query from the session session(id, "prepared", <name>)
	src_subs = make_buffers(config->global_names.session, 3, session->session_id->buf_addr, "prepared", parse->dest);
	YDB_STRING_TO_BUFFER(parse->query, &sql_expression);

	// Add the new SQL query to the database
	status = ydb_set_s(&src_subs[0], 3, &src_subs[1], &sql_expression);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	free(z_status_value.buf_addr);

	// Some clients depend on getting the rows back here; parse the expression, but don't execute it
	response = make_parse_complete();
	send_message(session, (BaseMessage*)(&response->type));
	free(response);

	return 0;
}
