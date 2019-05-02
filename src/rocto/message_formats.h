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
#ifndef MESSAGE_FORMATS_H
#define MESSAGE_FORMATS_H

// Source: https://www.postgresql.org/docs/current/protocol-message-formats.html
// Structs annotated with // B are "backend" messages (sent from the backend)
// // F are "frontend" messages (send from the frontend, read by us)
// Several items are just "stubs"; copy-paste the name, but no implementation
// These start with //#
// Some structures containe variable-length strings; we declare pointers
//  to those (and other items) in the first part of the struct. All network
//  data should start storing/reading at offset(type)

enum PSQL_MessageTypes {
	PSQL_Authenication = 'R',
	PSQL_AuthenticationMD5Password = 'R',
	PSQL_AuthenticationOk = 'R',
	PSQL_Bind = 'B',
	PSQL_BindComplete = '2',
	PSQL_Close = 'C',
	PSQL_CloseComplete = '3',
	PSQL_CommandComplete = 'C',
	PSQL_DataRow = 'D',
	PSQL_Describe = 'D',
	PSQL_EmptyQueryResponse = 'I',
	PSQL_ErrorResponse = 'E',
	PSQL_Execute = 'E',
	PSQL_NoData = 'n',
	PSQL_NoticeResponse = 'N',
	PSQL_ParameterStatus = 'S',
	PSQL_Parse = 'P',
	PSQL_ParseComplete = '1',
	PSQL_PasswordMessage = 'p',
	PSQL_Query = 'Q',
	PSQL_ReadyForQuery = 'Z',
	PSQL_RowDescription = 'T',
	PSQL_PortalSuspended = 's',
	PSQL_Sync = 'S',
	PSQL_Terminate = 'X'
};

typedef struct __attribute__((packed)) {
		char type;
		int length;
		char data[];
} BaseMessage;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
	int result;
} AuthenticationOk;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
	int _b;
} AuthenticationCleartextPassword;

// B
//# AuthenticationKerberosV5;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
	unsigned int md5_required;
	char salt[4];
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
//# BackendKeyData

typedef struct __attribute__((packed)) {
	int length;
	void *value;
} BindParm;

// F
typedef struct __attribute__((packed)) {
	char *dest;
	char *source;
	short int num_parm_format_codes;
	short int *parm_format_codes;
	short int num_parms;
	BindParm *parms;
	short int num_result_col_format_codes;
	short int *result_col_format_codes;

	char type;
	unsigned int length;
	char data[];
} Bind;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
} BindComplete;

// F
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
	char item;
	char data[];
} Close;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
} CloseComplete;

// B
typedef struct __attribute__((packed)) {
  char *command_tag;

  char type;
  unsigned int length;
  char data[];
} CommandComplete;

typedef struct {
	unsigned int length;
	char *value;
} DataRowParm;

// B
typedef struct __attribute__((packed)) {
	DataRowParm *parms;

	char type;
	unsigned int length;
	short num_columns;
	char data[];
} DataRow;

// F
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
	char item;
	char name[];
} Describe;

typedef struct {
	char type;
	char *value;
} ErrorResponseArg;

// B
typedef struct __attribute__((packed)) {
	ErrorResponseArg *args;

	char type;
	unsigned int length;
	char data[];
} ErrorResponse;

// F
typedef struct __attribute__((packed)) {
	char *source;
	unsigned int rows_to_return;

	char type;
	unsigned int length;
	char data[];
} Execute;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
} EmptyQueryResponse;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
} NoData;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
	char data[];
} ParameterStatus;

// F
typedef struct __attribute__((packed)) {
	char *dest;
	char *query;
	short num_parm_data_types;
	unsigned int *parm_data_types;

	char type;
	unsigned int length;
	char data[];
} Parse;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
} ParseComplete;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
} PortalSuspended;

// F
typedef struct __attribute__((packed)) {
	char *query;
	char type;
	unsigned int length;
	char data[];
} Query;

// F
typedef struct __attribute__((packed)) {
	char *password;
	char type;
	unsigned int length;
	char data[];
} PasswordMessage;

// B
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
	char status;
} ReadyForQuery;

// This must be packed because we use it to calculate size
//  of the RowDescription
typedef struct __attribute__((packed)) {
	char *name;
	int table_id;
	short column_id;
	int data_type;
	short data_type_size;
	int type_modifier;
	short format_code;
} RowDescriptionParm;

// B
typedef struct __attribute__((packed)) {
	short num_fields;
	RowDescriptionParm *parms;

	char type;
	unsigned int length;
	short num_parms;
	char data[];
} RowDescription;

typedef struct {
	char *name;
	char *value;
} StartupMessageParm;

// F
// This message is a special case; it's two hard-coded integers, followed by a series of
//  string => string mappings
typedef struct __attribute__((packed)) {
	StartupMessageParm *parameters;
	int num_parameters;

	unsigned int length;
	int protocol_version;
	char data[];
} StartupMessage;

// F
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
} Sync;

// F
typedef struct __attribute__((packed)) {
	char type;
	unsigned int length;
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

#define ERROR_DEF(name, format_string) name,
#define ERROR_END(name, format_string) name
typedef enum {
  #include "error_severity.hd"
} PSQL_ErrorSeverity;
#undef ERROR_DEF
#undef ERROR_END

#define ERROR_DEF(name, format_string) format_string,
#define ERROR_END(name, format_string) format_string
static const char *psql_error_severity_str[] = {
  #include "error_severity.hd"
};
#undef ERROR_DEF
#undef ERROR_END

#define ERROR_DEF(name, format_string) name,
#define ERROR_END(name, format_string) name
typedef enum {
  #include "error_codes.hd"
} PSQL_SQLSTATECode;
#undef ERROR_DEF
#undef ERROR_END

#define ERROR_DEF(name, format_string) format_string,
#define ERROR_END(name, format_string) format_string
static const char *psql_sqlstate_codes_str[] = {
  #include "error_codes.hd"
};
#undef ERROR_DEF
#undef ERROR_END

typedef enum {
	      PSQL_TransactionStatus_IDLE = 'I',
	      PSQL_TransactionStatus_TRANSACTION = 'T',
	      PSQL_TransactionStatus_FAILED = 'E'
} PSQL_TransactionStatus;

#endif
