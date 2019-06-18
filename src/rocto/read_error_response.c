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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

ErrorResponse *read_error_response(BaseMessage *message, ErrorResponse **err) {
	ErrorResponse *ret = NULL;
	char *cur_pointer = NULL, *last_byte = NULL;
	unsigned int remaining_length = 0, num_args = 0, i = 0;

	UNUSED(err);

	remaining_length = ntohl(message->length);
	ret = (ErrorResponse*)malloc(remaining_length + sizeof(ErrorResponse) - sizeof(unsigned int));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(unsigned int);
	memcpy(ret->data, message->data, remaining_length);
	cur_pointer = ret->data;
	last_byte = ret->data + remaining_length;

	// Count number of arguments
	while (cur_pointer < last_byte) {
		cur_pointer++;		// skip type indicator
		while (cur_pointer != last_byte && '\0' != *cur_pointer) {
			cur_pointer++;
		}
		cur_pointer++;		// skip null terminator
		num_args++;
	}

	ret->args = (ErrorResponseArg*)malloc(num_args * sizeof(ErrorResponseArg));

	// Populate args with type info and pointers into data section
	cur_pointer = ret->data;
	for (i = 0; i < num_args; i++) {
		ret->args[i].type = *cur_pointer;
		cur_pointer++;
		ret->args[i].value = cur_pointer;
		while (cur_pointer != last_byte && '\0' != *cur_pointer) {
			cur_pointer++;
		}
		cur_pointer++;		// skip null terminator
	}

	return ret;
}
