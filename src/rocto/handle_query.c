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
#include "physical_plan.h"

int no_more() {
	eof_hit = EOF_CTRLD;
	return 0;
}

int handle_query(Query *query, RoctoSession *session) {
	ParseContext	    parse_context;
	QueryResponseParms  parms;
	EmptyQueryResponse *empty_query_response;
	CommandComplete *   response;
	int32_t		    query_length = 0, run_query_result = 0;

	TRACE(INFO_ENTERING_FUNCTION, "handle_query");

	memset(&parse_context, 0, sizeof(ParseContext));
	memset(&parms, 0, sizeof(QueryResponseParms));
	parms.session = session;
	query_length = query->length - sizeof(unsigned int);
	if (0 == query_length) {
		empty_query_response = make_empty_query_response();
		send_message(session, (BaseMessage *)(&empty_query_response->type));
		free(empty_query_response);
		return 1;
	} else if (OCTO_MAX_QUERY_LEN < query_length) {
		ERROR(ERR_ROCTO_QUERY_TOO_LONG, query_length, OCTO_MAX_QUERY_LEN);
		return 1;
	}
	COPY_QUERY_TO_INPUT_BUFFER(query->query, query_length, NEWLINE_NEEDED_FALSE);

	run_query_result = run_query(&handle_query_response, (void *)&parms, TRUE, &parse_context);
	if (-1 == run_query_result) {
		// Exit loop if query was interrupted
		eof_hit = EOF_CTRLD;
		return -1;
	} else if (0 != run_query_result) {
		return 1;
	}

	response = make_command_complete(parse_context.command_tag, parms.rows_sent);
	if (NULL != response) {
		send_message(session, (BaseMessage *)(&response->type));
		free(response);
	}

	// If no data was sent (CREATE, DELETE, INSERT statement), send something back
	if (!parms.data_sent) {
	} else {
		// All done!
	}
	return 0;
}
