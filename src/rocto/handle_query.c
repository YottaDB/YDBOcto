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
	eof_hit = 1;
	return 0;
}

int handle_query(Query *query, RoctoSession *session) {
	ParseContext		parse_context;
	QueryResponseParms	parms;
	EmptyQueryResponse	*empty_query_response;
	CommandComplete		*response;
	int32_t			query_length = 0, run_query_result = 0;
	char			*err_buff;
	size_t			err_buff_size;
	char			command_tag[MAX_TAG_LEN];

	TRACE(ERR_ENTERING_FUNCTION, "handle_query");

	memset(&parse_context, 0, sizeof(ParseContext));
	memset(&parms, 0, sizeof(QueryResponseParms));
	parms.session = session;
	query_length = query->length - sizeof(unsigned int);
	if (query_length == 0) {
		empty_query_response = make_empty_query_response();
		send_message(session, (BaseMessage*)(&empty_query_response->type));
		free(empty_query_response);
		return 1;
	}
	eof_hit = 0;
	// If the query is bigger than the buffer, we would need to copy data from
	//  the query to the buffer after each block is consumed, but right now
	//  this buffer is large enough that this won't happen
	// Enforced by this check
	if (query_length > cur_input_max) {
		ERROR(ERR_ROCTO_QUERY_TOO_LONG, "");
		return 1;
	}
	memcpy(input_buffer_combined, query->query, query_length);
	input_buffer_combined[query_length] = '\0';
	eof_hit = FALSE;
	cur_input_index = 0;
	cur_input_more = &no_more;
	err_buffer = open_memstream(&err_buff, &err_buff_size);

	run_query_result = run_query(&handle_query_response, (void*)&parms, TRUE, &parse_context);
	if (-1 == run_query_result) {
		// Exit loop if query was interrupted
		eof_hit = TRUE;
		return -1;
	} else if (0 != run_query_result) {
		fclose(err_buffer);
		ERROR(CUSTOM_ERROR, err_buff);

		free(err_buff);
		err_buffer = open_memstream(&err_buff, &err_buff_size);
		return 1;
	}

	// Only SELECT statements get command tags
	if (0 == strncmp(parse_context.command_tag, "SELECT", MAX_TAG_LEN)) {
		snprintf(command_tag, MAX_TAG_LEN + INT64_TO_STRING_MAX,  "%s %d", parse_context.command_tag, parms.rows_sent);
		response = make_command_complete(command_tag);
		send_message(session, (BaseMessage*)(&response->type));
		free(response);
	}

	// If no data was sent (CREATE, DELETE, INSERT statement), send something back
	if (!parms.data_sent) {
	} else {
		// All done!
	}
	return 0;
}
