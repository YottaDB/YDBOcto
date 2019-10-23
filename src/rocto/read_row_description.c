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

RowDescription *read_row_description(BaseMessage *message) {
	RowDescription *ret;
	char *cur_pointer;
	uint32_t remaining_length = 0;

	remaining_length = ntohl(message->length);
	ret = (RowDescription*)malloc(remaining_length + sizeof(RowDescription) - sizeof(uint32_t) - sizeof(int16_t));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(uint32_t);
	cur_pointer = message->data;

	ret->num_parms = ntohs(*(int16_t*)cur_pointer);
	remaining_length -= sizeof(int16_t);
	cur_pointer += sizeof(int16_t);

	memcpy(ret->data, cur_pointer, remaining_length);
	cur_pointer = ret->data;

	ret->parms = (RowDescriptionParm*)malloc(ret->num_parms * sizeof(RowDescriptionParm));
	for(int16_t i = 0; i < ret->num_parms; i++) {
		ret->parms[i].name = cur_pointer;
		cur_pointer += strlen(ret->parms[i].name);
		cur_pointer += sizeof(char);	// skip null

		// Copy values, converting them to host endianess
		ret->parms[i].table_id = ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].column_id = ntohs(*((int16_t*)cur_pointer));
		cur_pointer += sizeof(int16_t);
		ret->parms[i].data_type	= ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].data_type_size = ntohs(*((int16_t*)cur_pointer));
		cur_pointer += sizeof(int16_t);
		ret->parms[i].type_modifier = ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].format_code = ntohs(*((int16_t*)cur_pointer));
		cur_pointer += sizeof(int16_t);
	}

	return ret;
}
