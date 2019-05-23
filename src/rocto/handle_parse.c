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
#include "helpers.h"
#include "rocto.h"

int handle_parse(Parse *parse, RoctoSession *session) {
	// At the moment, we don't have "bound function"
	// This feature should be implemented before 1.0
	// For now, just a search-and-replace of anything starting with a '$'
	// This is not super great because it means one could have a SQLI attack
	int status;
	char parm_data_type[12];
	ydb_buffer_t *src_subs, **parm_type_subs, value_buffer;
	ydb_buffer_t sql_expression, z_status, z_status_value;
	ParseComplete *response;
	char buff[MAX_STR_CONST], buff2[MAX_STR_CONST];

	TRACE(ERR_ENTERING_FUNCTION, "handle_parse");

	// zstatus buffers
	YDB_LITERAL_TO_BUFFER("$ZSTATUS", &z_status);
	INIT_YDB_BUFFER(&z_status_value, MAX_STR_CONST);

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
