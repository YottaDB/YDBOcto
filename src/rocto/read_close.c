/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

Close *read_close(BaseMessage *message) {
	Close *	 ret;
	char *	 cur_pointer, *last_byte;
	uint32_t remaining_length;

	// Create Close struct and initialize ALL bytes to prevent leaks
	remaining_length = ntohl(message->length);
	ret = (Close *)malloc(remaining_length + sizeof(char));
	memset(ret, 0, remaining_length + sizeof(char));

	// Copy entire message, including byte for the type field
	memcpy(&ret->type, message, remaining_length + sizeof(char));
	// The data section doesn't include the length or format code
	remaining_length -= sizeof(uint32_t); // Exclude length
	remaining_length -= sizeof(char);     // Exclude item format

	// Ensure valid value for type field
	if (ret->type != PSQL_Close) {
		ERROR(ERR_ROCTO_INVALID_MESSAGE_TYPE, "", ret->type, PSQL_Close);
		free(ret);
		return NULL;
	}
	// Ensure valid value for item field
	if ('S' != ret->item && 'P' != ret->item) {
		ERROR(ERR_ROCTO_INVALID_CHAR_VALUE, "Close", "item", ret->item, "'S' or 'P'");
		free(ret);
		return NULL;
	}

	// Ensure we use the entire message
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;
	while (cur_pointer < last_byte && '\0' != *cur_pointer)
		cur_pointer++;
	cur_pointer++;
	// Check for missing null terminator
	if (cur_pointer > last_byte) {
		ERROR(ERR_ROCTO_MISSING_NULL, "Close", "data");
		free(ret);
		return NULL;
	}
	// Check for premature null terminator
	if (cur_pointer < last_byte) {
		ERROR(ERR_ROCTO_TRAILING_CHARS, "Close");
		free(ret);
		return NULL;
	}

	return ret;
}
