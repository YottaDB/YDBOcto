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

typedef struct {
	OctodSession *session;
	int data_sent;
} QueryResponseParms;

void handle_response(PhysicalPlan *plan, int cursor_id, void *_parms) {
	QueryResponseParms *parms = (QueryResponseParms*)_parms;
	OctodSession *session = parms->session;
	// Large chunks copied from print_temporary table, mostly ydb_buffer stuff
	/// WARNING: the ordering of these buffers is essential to the ydb calls;
	//   if altered, make sure the order is correct
	ydb_buffer_t ydb_buffers[9];
	ydb_buffer_t *cursor_b = &ydb_buffers[0], *cursor_id_b = &ydb_buffers[1],
		*keys_b = &ydb_buffers[2], *key_id_b = &ydb_buffers[3],
		*space_b = &ydb_buffers[4], *space_b2 = &ydb_buffers[5],
		*row_id_b = &ydb_buffers[6], *row_value_b = &ydb_buffers[7],
		*empty_buffer = &ydb_buffers[8];
	ydb_buffer_t z_status, z_status_value;
	PhysicalPlan *deep_plan = plan;
	char buffer[MAX_STR_CONST];
	int status;

	// Go through and make rows for each row in the output plan
	parms->data_sent = TRUE;

	// make_row_description()

	while(deep_plan->next != NULL) {
		deep_plan = deep_plan->next;
	}

	YDB_LITERAL_TO_BUFFER("^cursor", cursor_b);

	snprintf(buffer, MAX_STR_CONST, "%d", cursor_id);
	cursor_id_b->len_used = strlen(buffer);
	cursor_id_b->buf_addr = malloc(cursor_id_b->len_used);
	memcpy(cursor_id_b->buf_addr, buffer, cursor_id_b->len_used+1);
	cursor_id_b->len_alloc = cursor_id_b->len_used;

	YDB_LITERAL_TO_BUFFER("keys", keys_b);

	snprintf(buffer, MAX_STR_CONST, "%d", deep_plan->outputKey->random_id);
	key_id_b->len_used = strlen(buffer);
	key_id_b->buf_addr = malloc(key_id_b->len_used);
	memcpy(key_id_b->buf_addr, buffer, key_id_b->len_used+1);
	key_id_b->len_alloc = key_id_b->len_used;

	YDB_LITERAL_TO_BUFFER(" ", space_b);
	YDB_LITERAL_TO_BUFFER(" ", space_b2);
	INIT_YDB_BUFFER(row_id_b, MAX_STR_CONST);

	YDB_LITERAL_TO_BUFFER("", empty_buffer);
	INIT_YDB_BUFFER(row_value_b, MAX_STR_CONST);

	status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
	if(status == YDB_ERR_NODEEND) {
		return;
	}
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	while(!YDB_BUFFER_IS_SAME(empty_buffer, row_id_b)) {
		status = ydb_get_s(cursor_b, 6, cursor_id_b, row_value_b);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		row_value_b->buf_addr[row_value_b->len_used] = '\0';
		// make_data_row()
		status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
		if(status == YDB_ERR_NODEEND) {
			break;
		}
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	}
	free(cursor_id_b->buf_addr);
	free(key_id_b->buf_addr);
	free(row_id_b->buf_addr);
	free(row_value_b->buf_addr);
	// make_command_complete()
	return;
}

int handle_query(Query *query, OctodSession *session) {
	QueryResponseParms parms;
	EmptyQueryResponse *empty_query_response;
	ErrorResponse *err;
	char input_buffer[MAX_STR_CONST], *c, *end_query;
	int query_length = 0;
	int run_query_result = 0;
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
	cur_input_more = &no_more;

	do {
		run_query_result = run_query(input_buffer, &handle_response, (void*)&parms);
	} while(!eof_hit);

	// If no data was sent (CREATE, DELETE, INSERT statement), send something back
	if(!parms.data_sent) {
	} else {
		// All done!
	}
	return 0;
}
