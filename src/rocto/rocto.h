/* Copyright (C) 2018 YottaDB, LLC
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
#ifndef OCTOD_H
#define OCTOD_H


#include <libyottadb.h>

#include "physical_plan.h"
#include "message_formats.h"

typedef struct {
	int connection_fd;
	ydb_buffer_t *session_id;
} RoctoSession;

typedef struct {
	RoctoSession *session;
	int data_sent;
	int max_data_to_send;
} QueryResponseParms;

int send_message(RoctoSession *session, BaseMessage *message);
BaseMessage *read_message(RoctoSession *session, char *buffer, int buffer_size);
int read_bytes(RoctoSession *session, char *buffer, int buffer_size, int bytes_to_read);
int rocto_main_loop(RoctoSession *session);
void free_error_response(ErrorResponse *err);

// make_* produces a structure for the correct type, taking in arguments specific to that type
// These should be viewed as helpers for other functions
// varargs should be of type ErrorResponseArg
ErrorResponse *make_error_response(PSQL_ErrorSeverity severity, PSQL_SQLSTATECode code, char *message, size_t num_args, ...);
BindComplete *make_bind_complete();
ReadyForQuery *make_ready_for_query(PSQL_TransactionStatus status);
EmptyQueryResponse *make_empty_query_response();
RowDescription *make_row_description(RowDescriptionParm *parms, short num_parms);
DataRow *make_data_row(DataRowParm *parms, short num_parms);
CommandComplete *make_command_complete(char *command_tag);
AuthenticationMD5Password *make_authentication_md5_password();
AuthenticationOk *make_authentication_ok();
ParseComplete *make_parse_complete();

// read_* messages parse the message and return a pointer to the filled out message type
// If the message was invalid, the return is NULL and *err is populated with an error message
Bind *read_bind(BaseMessage *message, ErrorResponse **err);
Query *read_query(BaseMessage *message, ErrorResponse **err);
Parse *read_parse(BaseMessage *message, ErrorResponse **err);
Execute *read_execute(BaseMessage *message, ErrorResponse **err);
Sync *read_sync(BaseMessage *message, ErrorResponse **err);
Describe *read_describe(BaseMessage *message, ErrorResponse **err);

// This is a special case because it must read more from the buffer
StartupMessage *read_startup_message(RoctoSession *session, char *data, int data_length, ErrorResponse **err);

// handle_* messages respond to a message of a given type, using send_message if needed
//  and returns 0 if the exchange is a "success", or non-zero if there was a problem
// A return of 1 means "done" and that we should close the session
int handle_bind(Bind *bind, RoctoSession *session);
int handle_query(Query *query, RoctoSession *session);
int handle_parse(Parse *parse, RoctoSession *session);
int handle_execute(Execute *execute, RoctoSession *session);
int handle_describe(Describe *describe, RoctoSession *session);

// This isn't a handle function in-of itself, but a helper to handle the results of a query
void handle_query_response(SqlStatement *stmt, PhysicalPlan *plan, int cursor_id, void *_parms);

// Helper to indicate that there is no more input
int no_more();

/**
 * Returns a RowDescription object for sending based on the provided physical plan
 */
RowDescription *get_plan_row_description(PhysicalPlan *plan);

#endif
