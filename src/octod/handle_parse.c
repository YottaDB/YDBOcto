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

int handle_parse(Parse *parse, OctodSession *session) {
	// At the moment, we don't have "bound function"
	// This feature should be implemented before 1.0
	// For now, just a search-and-replace of anything starting with a '$'
	// This is not super great because it means one could have a SQLI attack
	ydb_buffer_t subs_array[3], dest_subs[3];
	ydb_buffer_t session_global, sql_expression, *source_name = &subs_array[2], *prepared = &subs_array[1], *source_session_id = &subs_array[0];
	ydb_buffer_t *dest_session_id = &dest_subs[0], *bound = &dest_subs[1], *parse_name = &dest_subs[2];
	ydb_buffer_t z_status, z_status_value;
	size_t new_length = 0;
	int done = FALSE, length, status, parse_parm;
	char *ptr, *end_ptr, new_query[MAX_STR_CONST];
	char *int_start, *new_query_ptr, *end_new_query_ptr;
	char *new_value_start, *new_value_end, c;
	ParseComplete *response;
	ErrorResponse *err;

	TRACE(ERR_ENTERING_FUNCTION, "handle_parse");

	// zstatus buffers
	YDB_LITERAL_TO_BUFFER("$ZSTATUS", &z_status);
	INIT_YDB_BUFFER(&z_status_value, MAX_STR_CONST);

	// Fetch the named SQL query from the session ^session(id, "prepared", <name>)
	YDB_LITERAL_TO_BUFFER("^session", &session_global);
	INIT_YDB_BUFFER(source_session_id, session->session_id->len_used);
	YDB_COPY_BUFFER_TO_BUFFER(session->session_id, source_session_id, done);
	assert(done == TRUE);
	YDB_LITERAL_TO_BUFFER("prepared", prepared);
	source_name->buf_addr = parse->dest;
	source_name->len_used = source_name->len_alloc = strlen(parse->dest);

	sql_expression.buf_addr = parse->query;
	sql_expression.len_used = sql_expression.len_alloc = strlen(sql_expression.buf_addr);

	status = ydb_set_s(&session_global, 3, subs_array, &sql_expression);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	response = make_parse_complete();
	send_message(session, (BaseMessage*)(&response->type));
	free(response);

	free(z_status_value.buf_addr);

	// All done!
	return 0;
}
