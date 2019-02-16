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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "octod.h"
#include "message_formats.h"

int octod_main_loop(OctodSession *session) {
	Query *query;
	ErrorResponse *err;
	BaseMessage *message;
	ReadyForQuery *ready_for_query;
	int result;
	char buffer[MAX_STR_CONST];

	while (TRUE) {
		// Send ready
		ready_for_query = make_ready_for_query(PSQL_TransactionStatus_IDLE);
		result = send_message(session, (BaseMessage*)(&ready_for_query->type));
		if(result)
			break;
		free(ready_for_query);
		message = read_message(session, buffer, MAX_STR_CONST);
		switch(message->type) {
		case PSQL_Bind:
			break;
		case PSQL_ErrorResponse:
			// We don't expect this, issue an error
			break;
		case PSQL_BindComplete:
			break;
		case PSQL_ReadyForQuery:
			// We don't expect this, issue an error
			break;
		case PSQL_Query:
			query = read_query(message, &err);
			if(query == NULL) {
				send_message(session, (BaseMessage*)(&err->type));
				free_error_response(err);
				break;
			}
			result = handle_query(query, session);
			if(result == 1) {
				return 0;
			}
			break;
		case PSQL_EmptyQueryResponse:
			// We don't expect this, issue an error
			break;
		case PSQL_RowDescription:
			// We don't expect this, issue an error
			break;
		case PSQL_DataRow:
			// We don't expect this, issue an error
			break;
		case PSQL_CommandComplete:
			// We don't expect this, issue an error
			break;
		// All 3 of these have the same message code
		case PSQL_AuthenticationMD5Password:
		//case PSQL_Authenication:
		//case PSQL_AuthenticationOk:
			// We don't expect this, issue an error
			break;
		};
	}
	return 0;
}
