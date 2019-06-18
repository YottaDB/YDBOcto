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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

StartupMessage *read_startup_message(RoctoSession *session, char *data, int data_length, ErrorResponse **err) {
	StartupMessage *ret = NULL;
	ErrorBuffer err_buff;
	int num_parms = 0, cur_parm = 0;
	char *c, *message_end;
	const char *error_message;
	// Length plus protocol version
	unsigned int hard_coded_ints = sizeof(unsigned int) + sizeof(int);
	err_buff.offset = 0;

	// First read length and protocol type, then we will reallocate things
	ret = (StartupMessage*)malloc(sizeof(StartupMessage));
	memcpy(&ret->length, data, hard_coded_ints);
	ret->length = ntohl(ret->length);
	ret->protocol_version = ntohl(ret->protocol_version);

	// Protocol version number format:
	// 	most significant 16 bits:  major version #, i.e. 3
	// 	least significant 16 bits: minor version #, i.e. 0
	if(ret->protocol_version != 0x00030000) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_VERSION,
				"StartupMessage", ret->protocol_version, 0x00030000);
		*err = make_error_response(PSQL_Error_FATAL,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}

	// No parameters send
	if(ret->length == hard_coded_ints) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
		*err = make_error_response(PSQL_Error_FATAL,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}

	// Prepare to reallocate
	data_length = ret->length;
	free(ret);

	// Size is length in packet + other stuff in the struct, minus the hard-coded
	//  elements in the struct (two int4's)
	ret = (StartupMessage*)malloc(sizeof(StartupMessage) + data_length - hard_coded_ints);
	memcpy(&ret->length, data, hard_coded_ints);
	read_bytes(session, (char*)&ret->data, data_length - hard_coded_ints, data_length - hard_coded_ints);

	c = ret->data;
	message_end = (char*)(&ret->length) + data_length;

	// Fill out the list of messages; first, count them
	while(c < message_end && *c != '\0') {
		// Read parameter name
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		if(c == message_end || *c != '\0') {
			error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "StartupMessage", "parameter name");
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   error_message,
						   0);
			free(ret);
			return NULL;
		}
		c++;
		// Read parameter value
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		if(c == message_end || *c != '\0') {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "name or value");
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   error_message,
						   0);
			free(ret);
			return NULL;
		}
		c++;
		num_parms++;
	}
	// Ensure parameter list has null terminator
	if (c == message_end || *c != '\0') {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	// Skip over the ending \0
	c++;

	// If there are trailing characters, note it
	//  Right now, we will abort the startup, but it's possible to continue in this case
	if(c != message_end) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "StartupMessage");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}

	// Allocate parameter spots, reset c, scan through and populate data
	ret->parameters = (StartupMessageParm*)malloc(sizeof(StartupMessageParm) * num_parms);
	c = ret->data;
	cur_parm = 0;

	while(c < message_end && *c != '\0') {
		// Read parameter name
		ret->parameters[cur_parm].name = c;
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		// We already checked this message, so don't expect an error case
		assert(*c == '\0');
		c++;
		ret->parameters[cur_parm].value = c;
		// Read parameter value
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		assert(*c == '\0');
		c++;
		cur_parm++;
	}

	ret->num_parameters = cur_parm;

	return ret;
}
