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
#include "rocto.h"
#include "physical_plan.h"


int no_more() {
	eof_hit = 1;
	return 0;
}

int handle_query(Query *query, RoctoSession *session) {
	QueryResponseParms parms;
	EmptyQueryResponse *empty_query_response;
	int32_t query_length = 0;
	int32_t run_query_result = 0;
	char *err_buff;
	size_t err_buff_size;
	memset(&parms, 0, sizeof(QueryResponseParms));
	parms.session = session;
	query_length = query->length - sizeof(unsigned int);

	if(query_length == 0) {
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
	if(query_length > cur_input_max) {
		ERROR(ERR_ROCTO_QUERY_TOO_LONG, "");
		return 1;
	}
	memcpy(input_buffer_combined, query->query, query_length);
	input_buffer_combined[query_length] = '\0';
	eof_hit = FALSE;
	cur_input_index = 0;
	cur_input_more = &no_more;
	err_buffer = open_memstream(&err_buff, &err_buff_size);

	do {
		run_query_result = run_query(&handle_query_response, (void*)&parms);
		if (-1 == run_query_result) {
			// Exit loop if query was interrupted
			eof_hit = TRUE;
		} else if(0 != run_query_result) {
			fclose(err_buffer);
			ERROR(CUSTOM_ERROR, err_buff);

			free(err_buff);
			err_buffer = open_memstream(&err_buff, &err_buff_size);
		}
	} while(!eof_hit);

	// If no data was sent (CREATE, DELETE, INSERT statement), send something back
	if(!parms.data_sent) {
	} else {
		// All done!
	}
	return 0;
}
