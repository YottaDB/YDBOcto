#ifndef MESSAGE_FORMATS_H
#define MESSAGE_FORMATS_H

// Source: https://www.postgresql.org/docs/current/protocol-message-formats.html
// Structs annotated with // B are "backend" messages (sent from the backend)
// // F are "frontend" messages (send from the frontend, read by us)
// Several items are just "stubs"; copy-paste the name, but no implementation
// These start with //#
// Some structures containe variable-length strings; we declare pointers
//  to those (and other items) in the first part of the struct. All network
//  data should start storing at offset(type)

enum MessageTypes {
	PSQL_Authenication = 'R',
	PSQL_Bind = 'B'
};

typedef struct __attribute__((packed)) {
		char type;
		int length;
		char data[];
} BaseMessage;

// B
typedef struct __attribute__((packed)) {
	char type;
	int length;
	int result;
} AuthenticationOk;

//# AuthenticationKerberosV5;

// B
typedef struct __attribute__((packed)) {
	char type;
	int length;
	int _b;
} AuthenticationCleartextPassword;

//# AuthenticationMD5Password
//# AuthenticationSCMCredential
//# AuthenticationGSS
//# AuthenticationSSPI
//# AuthenticationGSSContinue
//# AuthenticationSASL
//# AuthenticationSASLContinue
//# AuthenticationSASLFinal

//# BackendKeyData

typedef struct {
	void *value;
	int length;
} BindParm;

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
	int length;
	char data[];
} Bind;

Bind *read_bind(BaseMessage *message);

#endif
