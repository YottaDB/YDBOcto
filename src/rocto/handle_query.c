/****************************************************************
 *								*
 * Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	*
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

int no_more(void) {
	eof_hit = EOF_CTRLD;
	return 0;
}

int handle_query(Query *query, RoctoSession *session) {
	ParseContext	    parse_context;
	QueryResponseParms  parms;
	EmptyQueryResponse *empty_query_response;
	CommandComplete	   *response;
	int32_t		    query_length = 0, run_query_result = 0, result = 0;

	TRACE(INFO_ENTERING_FUNCTION, "handle_query");

	query_length = query->length - sizeof(unsigned int);
	if (0 == query_length) {
		empty_query_response = make_empty_query_response();
		result = send_message(session, (BaseMessage *)(&empty_query_response->type));
		free(empty_query_response);
		return result;
	} else if (OCTO_MAX_QUERY_LEN < query_length) {
		ERROR(ERR_ROCTO_QUERY_TOO_LONG, query_length, OCTO_MAX_QUERY_LEN);
		return GENERIC_ERROR;
	}
	COPY_QUERY_TO_INPUT_BUFFER(query->query, query_length, NEWLINE_NEEDED_FALSE);

	do {
		memset(&parse_context, 0, sizeof(ParseContext));
		memset(&parms, 0, sizeof(QueryResponseParms));
		parms.session = session;
		run_query_result = run_query(&handle_query_response, (void *)&parms, PSQL_Query, &parse_context);
		if (QUERY_CANCELED == run_query_result) {
			// Exit loop if query was interrupted
			eof_hit = EOF_CTRLD;
			return QUERY_CANCELED;
		} else if (0 != run_query_result) {
			/* Return the status as is (instead of a flattened GENERIC_ERROR) so that a client disconnect
			 * detected while sending result rows (SOCK_OP_SHUTDOWN) reaches "rocto_main_loop()".
			 */
			return run_query_result;
		}

		response = make_command_complete(parse_context.command_tag, parms.row_count);
		if (NULL != response) {
			result = send_message(session, (BaseMessage *)(&response->type));
			free(response);
			if (SOCK_OP_SHUTDOWN == result) {
				return result;
			}
		}
	} while (EOF_NONE == eof_hit);

	return result;
}
