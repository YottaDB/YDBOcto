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

Parse *read_parse(BaseMessage *message) {
	Parse *	 ret;
	char *	 cur_pointer, *last_byte;
	uint32_t remaining_length;

	// Begin Parse initialization from message
	remaining_length = ntohl(message->length);
	ret = (Parse *)malloc(remaining_length + sizeof(Parse));
	memset(ret, 0, remaining_length + sizeof(Parse));
	memcpy(&ret->type, message, remaining_length + 1);

	// Ensure correct message type
	if (ret->type != PSQL_Parse) {
		ERROR(ERR_ROCTO_INVALID_MESSAGE_TYPE, "Parse", ret->type, PSQL_Parse);
		free(ret);
		return NULL;
	}

	// Continue initialization
	remaining_length -= 4;
	ret->dest = ret->data;
	// Utility pointers
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;

	// Ensure destination null terminated
	while (cur_pointer < last_byte && '\0' != *cur_pointer) {
		cur_pointer++;
	}
	if (cur_pointer == last_byte || '\0' != *cur_pointer) {
		ERROR(ERR_ROCTO_MISSING_NULL, "Parse", "destination");
		free(ret);
		return NULL;
	}
	// Ensure both dest and query fields included
	if (cur_pointer + 1 == last_byte) {
		ERROR(ERR_ROCTO_MISSING_DATA, "Parse", "destination or query");
		free(ret);
		return NULL;
	}
	cur_pointer++;
	ret->query = cur_pointer;
	// Ensure query null terminated
	while ('\0' != *cur_pointer && cur_pointer < last_byte) {
		cur_pointer++;
	}
	if ('\0' != *cur_pointer || cur_pointer == last_byte) {
		ERROR(ERR_ROCTO_MISSING_NULL, "Parse", "query");
		free(ret);
		return NULL;
	}
	cur_pointer++; // skip null terminator
	// Ensure number of parameter data types included
	if (cur_pointer == last_byte) {
		ERROR(ERR_ROCTO_MISSING_DATA, "Parse", "number of parameter data types");
		free(ret);
		return NULL;
	}
	ret->num_parm_data_types = ntohs(*((int16_t *)(cur_pointer)));
	// Ensure number of parameter data types valid
	if (0 > ret->num_parm_data_types) {
		ERROR(ERR_INVALID_NUMBER, "Parse", "parameter data types", ret->num_parm_data_types, 0, INT16_MAX);
		free(ret);
		return NULL;
	}
	cur_pointer += sizeof(int16_t);
	// Ensure parameter data types in bounds
	if (cur_pointer + sizeof(uint32_t) * ret->num_parm_data_types < last_byte) {
		ERROR(ERR_ROCTO_TOO_MANY_VALUES, "Parse", "parameter data types");
		free(ret);
		return NULL;
	}
	// Ensure all parameter data types included
	if (cur_pointer + sizeof(uint32_t) * ret->num_parm_data_types > last_byte) {
		ERROR(ERR_ROCTO_MISSING_DATA, "Parse", "parameter data types");
		free(ret);
		return NULL;
	}
	// Allocate a new array and use memcpy to prevent alignment issues from compiler optimizations
	// caused by casting char * into uint32_t *.
	ret->parm_data_types = (uint32_t *)calloc(ret->num_parm_data_types, sizeof(uint32_t));
	memcpy(ret->parm_data_types, cur_pointer, ret->num_parm_data_types * sizeof(uint32_t));
	for (int16_t i = 0; i < ret->num_parm_data_types; i++) {
		ret->parm_data_types[i] = ntohl(*((int *)(&ret->parm_data_types[i])));
	}
	return ret;
}
