/* Copyright (C) 2018-2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "octod.h"
#include "physical_plan.h"


int no_more() {
	eof_hit = 1;
	return 0;
}

int handle_query(Query *query, OctodSession *session) {
	QueryResponseParms parms;
	EmptyQueryResponse *empty_query_response;
	ErrorResponse *err;
	char input_buffer[MAX_STR_CONST], *c, *end_query;
	int query_length = 0;
	int run_query_result = 0;
	char *err_buff;
	size_t err_buff_size;
	memset(&parms, 0, sizeof(QueryResponseParms));
	parms.session = session;
	query_length = strlen(query->query);

	if(query_length == 0) {
		empty_query_response = make_empty_query_response();
		send_message(session, (BaseMessage*)(&empty_query_response->type));
		free(empty_query_response);
		return 0;
	}
	c = query->query;
	end_query = c + query_length;
	eof_hit = 0;
	// If the query is bigger than the buffer, we would need to copy data from
	//  the query to the buffer after each block is consumed, but right now
	//  this buffer is large enough that this won't happen
	// Enforced by this check
	if(query_length > cur_input_max) {
		err = make_error_response(PSQL_Error_ERROR,
				PSQL_Code_Protocol_Violation,
				"query length exceeeded maximum size",
				0);
		send_message(session, (BaseMessage*)(&err->type));
		free_error_response(err);
		return 0;
	}
	memcpy(input_buffer_combined, query->query, query_length);
	input_buffer_combined[query_length] = '\0';
	eof_hit = FALSE;
	cur_input_index = 0;
	cur_input_more = &no_more;
	err_buffer = open_memstream(&err_buff, &err_buff_size);

	do {
		run_query_result = run_query(input_buffer_combined, &handle_query_response, (void*)&parms);
		if(run_query_result == FALSE) {
			fclose(err_buffer);
			err = make_error_response(PSQL_Error_ERROR,
					PSQL_Code_Syntax_Error,
					err_buff,
					0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
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
