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

RowDescription *read_row_description(BaseMessage *message, ErrorResponse **err) {
	RowDescription *ret;
	char *cur_pointer, *last_byte;
	unsigned int remaining_length = 0, i = 0, cur_length = 0;

	remaining_length = ntohl(message->length);
	ret = (RowDescription*)malloc(remaining_length + sizeof(RowDescription) - sizeof(unsigned int) - sizeof(short int));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(unsigned int);
	cur_pointer = message->data;

	ret->num_parms = ntohs(*(short int*)cur_pointer);
	remaining_length -= sizeof(short int);
	cur_pointer += sizeof(short int);

	memcpy(ret->data, cur_pointer, remaining_length);
	cur_pointer = ret->data;

	ret->parms = (RowDescriptionParm*)malloc(ret->num_parms * sizeof(RowDescriptionParm));
	for(i = 0; i < ret->num_parms; i++) {
		ret->parms[i].name = cur_pointer;
		cur_pointer += strlen(ret->parms[i].name);
		cur_pointer += sizeof(char);	// skip null

		// Copy values, converting them to host endianess
		ret->parms[i].table_id = ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].column_id = ntohs(*((short*)cur_pointer));
		cur_pointer += sizeof(short);
		ret->parms[i].data_type	= ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].data_type_size = ntohs(*((short*)cur_pointer));
		cur_pointer += sizeof(short);
		ret->parms[i].type_modifier = ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].format_code = ntohs(*((short*)cur_pointer));
		cur_pointer += sizeof(short);
	}

	return ret;
}
