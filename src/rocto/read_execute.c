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

Execute *read_execute(BaseMessage *message, ErrorResponse **err) {
	Execute *ret;
	ErrorBuffer err_buff;
	char *cur_pointer, *last_byte;
	const char *error_message;
	unsigned int remaining_length;
	err_buff.offset = 0;

	// Create Execute message and initialize ALL bytes
	remaining_length = ntohl(message->length);
	ret = (Execute*)malloc(remaining_length + sizeof(Execute));
	memset(ret, 0, remaining_length + sizeof(Execute));

	// Populate Execute message values
	memcpy(&ret->type, message, remaining_length + 1);	// Include type field
	remaining_length -= sizeof(unsigned int);	// Length field precedes data field
	cur_pointer = ret->data;
	ret->source = cur_pointer;
	last_byte = cur_pointer + remaining_length;

	// Ensure message has correct type
	if(ret->type != PSQL_Execute) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Execute", ret->type, PSQL_Execute);
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	// Ensure message has null terminator
	while(cur_pointer < last_byte &&  '\0' != *cur_pointer) {
		cur_pointer++;
	}
	if(cur_pointer == last_byte || '\0' != *cur_pointer) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Execute", "source");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;		// Skip over null pointer
	// Ensure rows_to_return field included
	if(cur_pointer + sizeof(unsigned int) > last_byte) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Execute", "number of rows to return");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	// Check for trailing characters
	if(cur_pointer != last_byte - sizeof(unsigned int)) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "Execute");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	ret->rows_to_return = ntohl(*((unsigned int*)cur_pointer));

	return ret;
}
