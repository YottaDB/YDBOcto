/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

#define IS_ROCTO

#include <openssl/md5.h>

#include "errors.h"
#include "physical_plan.h"
#include "message_formats.h"
#include "constants.h"

#if YDB_TLS_AVAILABLE
#include "ydb_tls_interface.h"
#endif

/* The PostgreSQL wire protocol specifies that SQL NULL values are signaled to the client by returning a row length of -1.
 * See the `DataRow` entry at: https://www.postgresql.org/docs/11/protocol-message-formats.html
 */
#define PSQL_NULL -1

#define PORTAL_SUSPENDED -2

// Define column format codes for use when returning row data.
// For the derivation of these values, see the `Bind` entry at https://www.postgresql.org/docs/11/protocol-message-formats.html
#define TEXT_FORMAT   0
#define BINARY_FORMAT 1

/* Length required to store an MD5 hash digest as a hexidecimal string, including NULL terminator.
 * This value should be 33 = (16 * 2) + 1, per the MD5 specification which stipulates a digest length of 128 bits (16 bytes).
 */
#define MD5_HEX_LEN ((MD5_DIGEST_LENGTH * 2) + 1)

// The string 'md5', which is prefixed to password hashes per https://www.postgresql.org/docs/11/protocol-flow.html
#define MD5_PREFIX "md5"
// Length of the string 'md5', which is prefixed to password hashes per https://www.postgresql.org/docs/11/protocol-flow.html
#define MD5_PREFIX_LEN sizeof(MD5_PREFIX) - 1

typedef struct {
	int32_t	      connection_fd;
	int32_t	      sending_message;
	char *	      ip;
	char *	      port;
	ydb_buffer_t *session_id;
	int32_t	      session_ending;
	int32_t	      ssl_active;
	int32_t	      pid;
	int32_t	      secret_key;
#if YDB_TLS_AVAILABLE
	gtm_tls_socket_t *tls_socket;
#endif
} RoctoSession;

typedef struct {
	RoctoSession *session;
	int32_t	      data_sent;
	int32_t	      max_data_to_send;
	int32_t	      row_count;
	char *	      parm_name; /* used to store portal name, prepared statement name, variable name in case of SHOW etc. */
	char *	      command_type;
} QueryResponseParms;

void *rocto_helper_waitpid(void *args);

typedef enum UserColumns {
	UserColumn_OID,
	UserColumn_ROLNAME,
	UserColumn_ROLSUPER,
	UserColumn_ROLINHERIT,
	UserColumn_ROLCREATEROLE,
	UserColumn_ROLCREATEDB,
	UserColumn_ROLCANLOGIN,
	UserColumn_ROLREPLICATION,
	UserColumn_ROLBYPASSRLS,
	UserColumn_ROLCONNLIMIT,
	UserColumn_ROLPASSWORD,
	UserColumn_ROLVALIDUNTIL
} UserColumns;

int32_t	     send_message(RoctoSession *session, BaseMessage *message);
int32_t	     send_bytes(RoctoSession *session, char *message, size_t length);
BaseMessage *read_message(RoctoSession *session, char **buffer, int32_t *buffer_size, int32_t *rocto_err);
int32_t	     read_bytes(RoctoSession *session, char **buffer, int32_t *buffer_size, int32_t bytes_to_read, boolean_t allow_resize);
int32_t	     rocto_main_loop(RoctoSession *session);
void	     free_error_response(ErrorResponse *err);
void	     free_data_row(DataRow *drow);
void	     free_row_description(RowDescription *rowd);

// make_* produces a structure for the correct type, taking in arguments specific to that type
// These should be viewed as helpers for other functions
// varargs should be of type ErrorResponseArg
ErrorResponse * make_error_response(PSQL_ErrorSeverity severity, PSQL_SQLSTATECode code, const char *message, size_t num_args, ...);
BindComplete *	make_bind_complete();
BackendKeyData *make_backend_key_data(int32_t secret_key, pid_t pid);
CloseComplete * make_close_complete();
ReadyForQuery * make_ready_for_query(PSQL_TransactionStatus status);
EmptyQueryResponse *	   make_empty_query_response();
RowDescription *	   make_row_description(RowDescriptionParm *parms, int16_t num_parms);
DataRow *		   make_data_row(DataRowParm *parms, int16_t num_parms, int32_t *col_data_types);
CommandComplete *	   make_command_complete(SqlStatementType type, int32_t num_rows);
AuthenticationMD5Password *make_authentication_md5_password(RoctoSession *session, char *salt);
AuthenticationOk *	   make_authentication_ok();
ParseComplete *		   make_parse_complete();
ParameterStatus *	   make_parameter_status(StartupMessageParm *parm);
PortalSuspended *	   make_portal_suspended();
NoData *		   make_no_data();
ParameterDescription *	   make_parameter_description();

// read_* messages parse the message and return a pointer to the filled out message type
// If the message was invalid, the return is NULL
Bind *		 read_bind(BaseMessage *message);
Close *		 read_close(BaseMessage *message);
Flush *		 read_flush(BaseMessage *message);
Query *		 read_query(BaseMessage *message);
Parse *		 read_parse(BaseMessage *message);
PasswordMessage *read_password_message(BaseMessage *message);
Execute *	 read_execute(BaseMessage *message);
Sync *		 read_sync(BaseMessage *message);
Describe *	 read_describe(BaseMessage *message);

// This is a special case because it must read more from the buffer
StartupMessage *read_startup_message(RoctoSession *session, char **data, int32_t *data_length);
SSLRequest *	read_ssl_request(RoctoSession *session, char *data, int32_t data_length);
CancelRequest * read_cancel_request(RoctoSession *session, char *data, uint32_t data_size);

// handle_* messages respond to a message of a given type, using send_message if needed
//  and returns 0 if the exchange is a "success", or non-zero if there was a problem
// A return of 1 means "done" and that we should close the session
int32_t handle_bind(Bind *bind, RoctoSession *session);
int32_t handle_cancel_request(CancelRequest *cancel_request);
int32_t handle_query(Query *query, RoctoSession *session);
int32_t handle_parse(Parse *parse, RoctoSession *session);
int32_t handle_execute(Execute *execute, RoctoSession *session, ydb_long_t *cursorId);
int32_t handle_describe(Describe *describe, RoctoSession *session);
int32_t handle_password_message(PasswordMessage *password_message, StartupMessage *startup_message, char *salt);

// This isn't a handle function in-of itself, but a helper to handle the results of a query
int handle_query_response(SqlStatement *stmt, ydb_long_t cursorId, void *_parms, char *plan_name, PSQL_MessageTypeT msg_type);
// Returns result rows from plan_name stored on cursorId
int send_result_rows(ydb_long_t cursorId, void *_parms, char *plan_name);

// Helper to indicate that there is no more input
int no_more();

// Helpers to deserialize binary data
int64_t ntoh64(int64_t little_endian);
int64_t hton64(int64_t little_endian);
char *	byte_to_hex(char c, char *hex);
int32_t md5_to_hex(const unsigned char *md5_hash, char *hex, uint32_t hex_len);
int64_t bin_to_bool(char *bin);
int64_t bin_to_char(char *bin);
int64_t bin_to_int16(char *bin);
int64_t bin_to_int32(char *bin);
int64_t bin_to_int64(char *bin);
int64_t bin_to_oid(char *bin);
// float bin_to_float4(char *bin);
// double bin_to_float8(char *bin);
char *bin_to_bytea(char *bin);
void  bin_to_uuid(char *bin, char *buffer);

// Utility functions for copying Bind parameters into query string
int32_t copy_binary_parameter(Bind *bind, const int32_t cur_parm, ydb_buffer_t *bound_query);
int32_t get_binary_parameter_length(Bind *bind, const int32_t cur_parm);

// Helper to extract column values from delimited row string
uint32_t get_user_column_value(char *buffer, const uint32_t buf_len, const char *row, const uint32_t row_len,
			       enum UserColumns column);
// Helper to retrieve the time a process with specified pid was started, relative to system boot time
unsigned long long get_pid_start_time(pid_t pid);

/**
 * Returns a RowDescription object for sending based on the provided physical plan
 */
RowDescription *get_plan_row_description(ydb_buffer_t *plan_filename);

// Read functions to simulate client reception of the various make_* response messages. Used for testing.
// AuthenticationMD5Password *read_authentication_md5_password(BaseMessage *message);
AuthenticationOk *  read_authentication_ok(BaseMessage *message);
BindComplete *	    read_bind_complete(BaseMessage *message);
CloseComplete *	    read_close_complete(BaseMessage *message);
CommandComplete *   read_command_complete(BaseMessage *message);
DataRow *	    read_data_row(BaseMessage *message);
EmptyQueryResponse *read_empty_query_response(BaseMessage *message);
ParameterStatus *   read_parameter_status(BaseMessage *message);
ParseComplete *	    read_parse_complete(BaseMessage *message);
PortalSuspended *   read_portal_suspended(BaseMessage *message);
ReadyForQuery *	    read_ready_for_query(BaseMessage *message);
RowDescription *    read_row_description(BaseMessage *message);

// Globals
extern RoctoSession rocto_session;

#endif
