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

#ifndef MESSAGE_FORMATS_H
#define MESSAGE_FORMATS_H

#include <libyottadb.h>
#include "octo_types.h"

// Source: https://www.postgresql.org/docs/current/protocol-message-formats.html
// Structs annotated with // B are "backend" messages (sent from the backend)
// // F are "frontend" messages (send from the frontend, read by us)
// Several items are just "stubs"; copy-paste the name, but no implementation
// These start with //#
// Some structures contain variable-length strings; we declare pointers
//  to those (and other items) in the first part of the struct. All network
//  data should start storing/reading at offset(type)

typedef struct __attribute__((packed)) {
	char	type;
	int32_t length;
	char	data[];
} BaseMessage;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	int32_t	 result;
} AuthenticationOk;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	int32_t	 _b;
} AuthenticationCleartextPassword;

// B
//# AuthenticationKerberosV5;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	uint32_t md5_required;
	char	 salt[4];
} AuthenticationMD5Password;

// B
//# AuthenticationGSS
//# AuthenticationSSPI
//# AuthenticationGSSContinue
//# AuthenticationSASL
//# AuthenticationSASLContinue
//# AuthenticationSASLFinal
//# AuthenticationSCMCredential

// B
typedef struct __attribute__((packed)) {
	char	     type;
	unsigned int length;
	pid_t	     pid;
	unsigned int secret_key;
} BackendKeyData;

typedef struct __attribute__((packed)) {
	int32_t length;
	void *	value;
} BindParm;

// F
typedef struct __attribute__((packed)) {
	ParseContext parse_context;
	char *	     dest;
	char *	     source;
	int16_t	     num_parm_format_codes;
	int16_t *    parm_format_codes;
	int16_t	     num_parms;
	BindParm *   parms;
	int16_t	     num_result_col_format_codes;
	int16_t *    result_col_format_codes;

	char	 type;
	uint32_t length;
	char	 data[];
} Bind;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} BindComplete;

// F
typedef struct __attribute__((packed)) {
	uint32_t length;
	int32_t	 request_code;
	pid_t	 pid;
	uint32_t secret_key;
} CancelRequest;

// F
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	char	 item;
	char	 data[];
} Close;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} CloseComplete;

// B
typedef struct __attribute__((packed)) {
	char *command_tag;

	char	 type;
	uint32_t length;
	char	 data[];
} CommandComplete;

typedef struct {
	int32_t length;
	char *	value;
	int16_t format;
	int32_t column_type;
} DataRowParm;

// B
typedef struct __attribute__((packed)) {
	DataRowParm *parms;

	char	 type;
	uint32_t length;
	int16_t	 num_columns;
	// If `length == 0`, it is undefined behavior to dereference this pointer.
	char data[];
} DataRow;

// F
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	char	 item;
	char	 name[];
} Describe;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} EmptyQueryResponse;

typedef struct {
	char  type;
	char *value;
} ErrorResponseArg;

// B
typedef struct __attribute__((packed)) {
	ErrorResponseArg *args;

	char	 type;
	uint32_t length;
	char	 data[];
} ErrorResponse;

// F
typedef struct __attribute__((packed)) {
	char *	 source;
	uint32_t rows_to_return;

	char	 type;
	uint32_t length;
	char	 data[];
} Execute;

// F
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} Flush;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} NoData;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	char	 data[];
} ParameterStatus;

// F
typedef struct __attribute__((packed)) {
	char *	  dest;
	char *	  query;
	int16_t	  num_parm_data_types;
	uint32_t *parm_data_types;

	char	 type;
	uint32_t length;
	char	 data[];
} Parse;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} ParseComplete;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} PortalSuspended;

// F
typedef struct __attribute__((packed)) {
	char *	 query;
	char	 type;
	uint32_t length;
	char	 data[];
} Query;

// F
typedef struct __attribute__((packed)) {
	char *	 password;
	char	 type;
	uint32_t length;
	char	 data[];
} PasswordMessage;

// B
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	char	 status;
} ReadyForQuery;

// This must be packed because we use it to calculate size
//  of the RowDescription
typedef struct __attribute__((packed)) {
	char *	name;
	int32_t table_id;
	int16_t column_id;
	int32_t data_type;
	int16_t data_type_size;
	int32_t type_modifier;
	int16_t format_code;
} RowDescriptionParm;

// B
typedef struct __attribute__((packed)) {
	int16_t		    num_fields;
	RowDescriptionParm *parms;

	char	 type;
	uint32_t length;
	int16_t	 num_parms;
	char	 data[];
} RowDescription;

typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
	int16_t	 num_parms;
	char	 data[];
} ParameterDescription;

// F
typedef struct __attribute__((packed)) {
	uint32_t length;
	int32_t	 request_code;
} SSLRequest;

typedef struct {
	char *name;
	char *value;
} StartupMessageParm;

// F
// This message is a special case; it's two hard-coded integers, followed by a series of
//  string => string mappings
typedef struct __attribute__((packed)) {
	StartupMessageParm *parameters;
	int32_t		    num_parameters;

	uint32_t length;
	int32_t	 protocol_version;
	char	 data[];
} StartupMessage;

// F
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} Sync;

// F
typedef struct __attribute__((packed)) {
	char	 type;
	uint32_t length;
} Terminate;

typedef enum {
	PSQL_Error_SEVERITY = 'S',
	PSQL_Error_Code = 'C',
	PSQL_Error_Message = 'M',
	PSQL_Error_Detail = 'D',
	PSQL_Error_Hint = 'H',
	PSQL_Error_Position = 'P',
	PSQL_Error_Internal_Position = 'p',
	PSQL_Error_Internal_Query = 'q',
	PSQL_Error_Where = 'W',
	PSQL_Error_Schema_Name = 's',
	PSQL_Error_Table_Name = 't',
	PSQL_Error_Column_Name = 'c',
	PSQL_Error_Data_Type_Name = 'd',
	PSQL_Error_Constraint_Name = 'n',
	PSQL_Error_File = 'F',
	PSQL_Error_Line = 'L',
	PSQL_Error_Routine = 'R'
} PSQL_ErrorResponseArgType;

extern const char *psql_error_severity_str[];
extern const char *psql_sqlstate_codes_str[];

#define ERROR_DEF(name, format_string) name,
#define ERROR_END(name, format_string) name
typedef enum {
#include "error_severity.hd"
} PSQL_ErrorSeverity;
#undef ERROR_DEF
#undef ERROR_END

typedef enum {
	PSQL_TransactionStatus_IDLE = 'I',
	PSQL_TransactionStatus_TRANSACTION = 'T',
	PSQL_TransactionStatus_FAILED = 'E'
} PSQL_TransactionStatus;

#endif
