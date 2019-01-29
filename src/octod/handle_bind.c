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

int handle_bind(Bind *bind, OctodSession *session) {
	// At the moment, we don't have "bound function"
	// This feature should be implemented before 1.0
	// For now, just a search-and-replace of anything starting with a '$'
	// This is not super great because it means one could have a SQLI attack
	ydb_buffer_t subs_array[3], dest_subs[3];
	ydb_buffer_t session_global, sql_expression, *source_name = &subs_array[2], *prepared = &subs_array[1], *source_session_id = &subs_array[0];
	ydb_buffer_t *dest_session_id = &dest_subs[0], *bound = &dest_subs[1], *bind_name = &dest_subs[2];
	ydb_buffer_t z_status, z_status_value;
	size_t new_length = 0;
	int done = FALSE, length, status, bind_parm;
	char *ptr, *end_ptr, new_query[MAX_STR_CONST];
	char *int_start, *new_query_ptr, *end_new_query_ptr;
	char *new_value_start, *new_value_end, c;
	BindComplete *response;

	// zstatus buffers
	YDB_LITERAL_TO_BUFFER("$ZSTATUS", &z_status);
	INIT_YDB_BUFFER(&z_status_value, MAX_STR_CONST);

	// Fetch the named SQL query from the session ^session(id, "prepared", <name>)
	YDB_LITERAL_TO_BUFFER("^session", &session_global);
	INIT_YDB_BUFFER(source_session_id, session->session_id->len_used);
	YDB_COPY_BUFFER_TO_BUFFER(session->session_id, source_session_id, done);
	assert(done == TRUE);
	YDB_LITERAL_TO_BUFFER("prepared", prepared);
	YDB_LITERAL_TO_BUFFER(bind->source, source_name);

	INIT_YDB_BUFFER(&sql_expression, MAX_STR_CONST);

	status = ydb_get_s(&session_global, 3, subs_array, &sql_expression);
	/// TODO: we should add an error check here for an invalid, non existing BIND option
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	// Scan through and calculate a new length for the query
	ptr = sql_expression.buf_addr;
	end_ptr = ptr + sql_expression.len_used;
	new_query_ptr = new_query;
	end_new_query_ptr = new_query + MAX_STR_CONST;
	while(ptr < end_ptr) {
		if(*ptr == '$') {
			// Extract the parameter number, then paste in the new value
			int_start = ++ptr;
			while(ptr < end_ptr && *ptr >= 48 && *ptr <= 57) {
				*ptr++;
			}
			c = *ptr;
			*ptr = '\0';
			bind_parm = atoi(int_start);
			*ptr = c;
			if(bind_parm == -1) {
				// Special case of a "NULL" value, which we represent as an
				//  empty string. This is technically incorrect, but life
				//  goes on
				*new_query_ptr++ = '"';
				*new_query_ptr++ = '"';
			} else {
				// Copy in the value; if the type is a string, wrap it in
				//  quotes, otherwise just place it
				assert(bind_parm < bind->num_parms);
				*new_query_ptr++ = '"';
				new_value_start = bind->parms[bind_parm].value;
				new_value_end = new_value_start + bind->parms[bind_parm].length;
				while(new_value_start < new_value_end) {
					*new_query_ptr++ = *new_value_start++;
					assert(new_query_ptr < end_new_query_ptr);
				}
				*new_query_ptr++ = '"';
				assert(new_query_ptr < end_new_query_ptr);
			}
		} else {
			*new_query_ptr++ = *ptr;
		}
		assert(new_query_ptr < end_new_query_ptr);
		ptr++;
	}

	// Now we store the bound statement in a global to execute ^session(session_id, "bound", <bound name>)
	INIT_YDB_BUFFER(dest_session_id, session->session_id->len_used);
	YDB_COPY_BUFFER_TO_BUFFER(session->session_id, dest_session_id, done);
	YDB_LITERAL_TO_BUFFER("bound", bound);
	YDB_LITERAL_TO_BUFFER(bind->dest, bind_name);
	bind_name->len_alloc = bind_name->len_used = strlen(bind->dest);

	sql_expression.buf_addr = new_query;
	sql_expression.len_alloc = MAX_STR_CONST;
	sql_expression.len_used = new_query_ptr - new_query;

	status = ydb_set_s(&session_global, 3, dest_subs, &sql_expression);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	// Construct the response message
	response = make_bind_complete();
	send_message(session, (BaseMessage*)response);

	// All done!
	return 0;
}
