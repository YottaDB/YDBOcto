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

#ifndef OCTOD_H
#define OCTOD_H

#include <libyottadb.h>

#define FEATURE_ROCTO

#include "errors.h"
#include "physical_plan.h"
#include "message_formats.h"
#include "constants.h"

#if YDB_TLS_AVAILABLE
#include "ydb_tls_interface.h"
#endif

typedef struct {
	int connection_fd;
	int sending_message;
	char *ip;
	char *port;
	ydb_buffer_t *session_id;
	int session_ending;
	int ssl_active;
#if YDB_TLS_AVAILABLE
	gtm_tls_socket_t *tls_socket;
#endif
} RoctoSession;

typedef struct {
	RoctoSession *session;
	int data_sent;
	int max_data_to_send;
} QueryResponseParms;

void *rocto_helper_waitpid(void *args);

enum UserColumns {
	OID,
	ROLNAME,
	ROLSUPER,
	ROLINHERIT,
	ROLCREATEROLE,
	ROLCREATEDB,
	ROLCANLOGIN,
	ROLREPLICATION,
	ROLBYPASSRLS,
	ROLCONNLIMIT,
	ROLPASSWORD,
	ROLVALIDUNTIL
} typedef UserColumns;

int send_message(RoctoSession *session, BaseMessage *message);
int send_bytes(RoctoSession *session, char *message, size_t length);
BaseMessage *read_message(RoctoSession *session, char *buffer, int buffer_size);
int read_bytes(RoctoSession *session, char *buffer, int buffer_size, int bytes_to_read);
int rocto_main_loop(RoctoSession *session);
void free_error_response(ErrorResponse *err);
void free_data_row(DataRow *drow);
void free_row_description(RowDescription *rowd);

// make_* produces a structure for the correct type, taking in arguments specific to that type
// These should be viewed as helpers for other functions
// varargs should be of type ErrorResponseArg
ErrorResponse *make_error_response(PSQL_ErrorSeverity severity, PSQL_SQLSTATECode code, const char *message, size_t num_args, ...);
BindComplete *make_bind_complete();
CloseComplete *make_close_complete();
ReadyForQuery *make_ready_for_query(PSQL_TransactionStatus status);
EmptyQueryResponse *make_empty_query_response();
RowDescription *make_row_description(RowDescriptionParm *parms, short num_parms);
DataRow *make_data_row(DataRowParm *parms, short num_parms);
CommandComplete *make_command_complete(char *command_tag);
AuthenticationMD5Password *make_authentication_md5_password(RoctoSession *session, char *salt);
AuthenticationOk *make_authentication_ok();
ParseComplete *make_parse_complete();
ParameterStatus *make_parameter_status(StartupMessageParm *parm);
PortalSuspended *make_portal_suspended();
NoData *make_no_data();
ParameterDescription *make_parameter_description();

// read_* messages parse the message and return a pointer to the filled out message type
// If the message was invalid, the return is NULL and *err is populated with an error message
Bind *read_bind(BaseMessage *message, ErrorResponse **err);
Close *read_close(BaseMessage *message, ErrorResponse **err);
Flush *read_flush(BaseMessage *message, ErrorResponse **err);
Query *read_query(BaseMessage *message, ErrorResponse **err);
Parse *read_parse(BaseMessage *message, ErrorResponse **err);
PasswordMessage *read_password_message(BaseMessage *message, ErrorResponse **err);
Execute *read_execute(BaseMessage *message, ErrorResponse **err);
Sync *read_sync(BaseMessage *message, ErrorResponse **err);
Terminate *read_terminate(BaseMessage *message, ErrorResponse **err);
Describe *read_describe(BaseMessage *message, ErrorResponse **err);

// This is a special case because it must read more from the buffer
StartupMessage *read_startup_message(RoctoSession *session, char *data, int data_length, ErrorResponse **err);
SSLRequest *read_ssl_request(RoctoSession *session, char *data, int data_length, ErrorResponse **err);

// handle_* messages respond to a message of a given type, using send_message if needed
//  and returns 0 if the exchange is a "success", or non-zero if there was a problem
// A return of 1 means "done" and that we should close the session
int handle_bind(Bind *bind, RoctoSession *session);
int handle_query(Query *query, RoctoSession *session);
int handle_parse(Parse *parse, RoctoSession *session);
int handle_execute(Execute *execute, RoctoSession *session);
int handle_describe(Describe *describe, RoctoSession *session);
int handle_password_message(PasswordMessage *password_message, RoctoSession *session, ErrorResponse **err, StartupMessage
		*startup_message, char *salt);

// This isn't a handle function in-of itself, but a helper to handle the results of a query
void handle_query_response(SqlStatement *stmt, int cursor_id, void *_parms, char *plan_name);

// Helper to indicate that there is no more input
int no_more();

// Helpers to deserialize binary data
int64_t ntoh64(int64_t little_endian);
int64_t hton64(int64_t little_endian);
char *byte_to_hex(char c, char *hex);
int md5_to_hex(char *md5_hash, char *hex, unsigned int hex_len);
int64_t bin_to_bool(char *bin);
int64_t bin_to_char(char *bin);
int64_t bin_to_int16(char *bin);
int64_t bin_to_int32(char *bin);
int64_t bin_to_int64(char *bin);
int64_t bin_to_oid(char *bin);
// float bin_to_float4(char *bin);
// double bin_to_float8(char *bin);
char *bin_to_bytea(char *bin);
void bin_to_uuid(char *bin, char *buffer, int buf_len);

// Utility functions for copying Bind parameters into query string
char *copy_text_parameter(RoctoSession *session, Bind *bind, const int cur_parm, char *query_ptr, const char *end_query_ptr);
char *copy_binary_parameter(RoctoSession *session, Bind *bind, const int cur_parm, char *query_ptr, const char *end_query_ptr);

// Helper to extract column values from delimited row string
unsigned int get_user_column_value(char *buffer, const unsigned int buf_len, const char *row, const unsigned int row_len,
		enum UserColumns column);

/**
 * Returns a RowDescription object for sending based on the provided physical plan
 */
RowDescription *get_plan_row_description(ydb_buffer_t *plan_filename);

// Read functions to simulate client reception of the various make_* response messages. Used for testing.
// AuthenticationMD5Password *read_authentication_md5_password(BaseMessage *message, ErrorResponse **err);
AuthenticationOk *read_authentication_ok(BaseMessage *message, ErrorResponse **err);
BindComplete *read_bind_complete(BaseMessage *message, ErrorResponse **err);
CloseComplete *read_close_complete(BaseMessage *message, ErrorResponse **err);
CommandComplete *read_command_complete(BaseMessage *message, ErrorResponse **err);
DataRow *read_data_row(BaseMessage *message, ErrorResponse **err);
EmptyQueryResponse *read_empty_query_response(BaseMessage *message, ErrorResponse **err);
ErrorResponse *read_error_response(BaseMessage *message, ErrorResponse **err);
ParameterStatus *read_parameter_status(BaseMessage *message, ErrorResponse **err);
ParseComplete *read_parse_complete(BaseMessage *message, ErrorResponse **err);
PortalSuspended *read_portal_suspended(BaseMessage *message, ErrorResponse **err);
ReadyForQuery *read_ready_for_query(BaseMessage *message, ErrorResponse **err);
RowDescription *read_row_description(BaseMessage *message, ErrorResponse **err);

// Make functions to simulate client transmission of certain make_* messages. Used for testing.
PasswordMessage *make_password_message(char *user, char *password, char *salt);

RoctoSession rocto_session;

#endif
