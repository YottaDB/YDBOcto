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

Describe *read_describe(BaseMessage *message) {
	Describe *ret;
	char *	  cur_pointer, *last_byte;
	uint32_t  remaining_length;

	// Create Describe struct and initialize ALL bytes to prevent leaks
	remaining_length = ntohl(message->length);
	ret = (Describe *)malloc(remaining_length + sizeof(Describe));
	memset(ret, 0, remaining_length + sizeof(Describe));

	// Copy entire message, including byte for the type field
	memcpy(&ret->type, message, remaining_length + 1);
	// The data section doesn't include the length or format code
	remaining_length -= sizeof(uint32_t); // Exclude length
	remaining_length -= sizeof(char);     // Exclude format

	// Ensure valid value for type field
	if (ret->type != 'D') {
		ERROR(ERR_ROCTO_INVALID_MESSAGE_TYPE, "Describe", ret->type, PSQL_Describe);
		free(ret);
		return NULL;
	}
	// Ensure valid value for item field
	if (ret->item != 'S' && ret->item != 'P') {
		ERROR(ERR_ROCTO_INVALID_CHAR_VALUE, "Describe", "item", ret->item, "'S' or 'P'");
		free(ret);
		return NULL;
	}

	// Ensure we use the entire message
	cur_pointer = ret->name;
	last_byte = cur_pointer + remaining_length;
	while ('\0' != *cur_pointer)
		cur_pointer++;
	cur_pointer++;
	// Check for missing null terminator
	if (cur_pointer > last_byte) {
		ERROR(ERR_ROCTO_MISSING_NULL, "Describe", "name");
		free(ret);
		return NULL;
	}
	// Check for premature null terminator
	if (cur_pointer < last_byte) {
		ERROR(ERR_ROCTO_TRAILING_CHARS, "Describe");
		free(ret);
		return NULL;
	}

	return ret;
}
