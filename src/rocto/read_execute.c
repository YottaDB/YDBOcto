/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

Execute *read_execute(BaseMessage *message) {
	Execute *ret;
	char	*cur_pointer, *last_byte;
	uint32_t remaining_length;

	// Create Execute message and initialize ALL bytes
	remaining_length = ntohl(message->length);
	ret = (Execute *)malloc(remaining_length + sizeof(Execute));
	memset(ret, 0, remaining_length + sizeof(Execute));

	// Populate Execute message values
	memcpy(&ret->type, message, remaining_length + 1); // Include type field
	remaining_length -= sizeof(uint32_t);		   // Length field precedes data field
	cur_pointer = ret->data;
	ret->source = cur_pointer;
	last_byte = cur_pointer + remaining_length;

	// Ensure message has correct type
	if (ret->type != PSQL_Execute) {
		ERROR(ERR_ROCTO_INVALID_MESSAGE_TYPE, "Execute", ret->type, PSQL_Execute);
		free(ret);
		return NULL;
	}
	// Ensure message has null terminator
	while (cur_pointer < last_byte && '\0' != *cur_pointer) {
		cur_pointer++;
	}
	if (cur_pointer == last_byte || '\0' != *cur_pointer) {
		ERROR(ERR_ROCTO_MISSING_NULL, "Execute", "source");
		free(ret);
		return NULL;
	}
	cur_pointer++; // Skip over null pointer
	// Ensure rows_to_return field included
	if (cur_pointer + sizeof(uint32_t) > last_byte) {
		ERROR(ERR_ROCTO_MISSING_DATA, "Execute", "number of rows to return");
		free(ret);
		return NULL;
	}
	// Check for trailing characters
	if (cur_pointer != last_byte - sizeof(uint32_t)) {
		ERROR(ERR_ROCTO_TRAILING_CHARS, "Execute");
		free(ret);
		return NULL;
	}
	ret->rows_to_return = ntohl(*((uint32_t *)cur_pointer));

	return ret;
}
