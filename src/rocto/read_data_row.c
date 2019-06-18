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

DataRow *read_data_row(BaseMessage *message, ErrorResponse **err) {
	DataRow *ret;
	char *cur_pointer, *c;
	unsigned int remaining_length = 0;

	UNUSED(err);

	remaining_length = ntohl(message->length);
	ret = (DataRow*)malloc(remaining_length + sizeof(DataRow) - sizeof(unsigned int) - sizeof(short int));
	// Exclude DataRowParm array pointer
	memset(&ret->type, 0, remaining_length + sizeof(DataRow) - sizeof(unsigned int) - sizeof(short int) - sizeof(DataRowParm*));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(unsigned int);
	c = message->data;
	ret->num_columns = ntohs(*(short*)c);
	if (ret->num_columns == 0) {
		ret->parms = NULL;
		return ret;
	}

	remaining_length -= sizeof(short int);
	c += sizeof(short int);
	memcpy(ret->data, c, remaining_length);
	ret->parms = (DataRowParm*)malloc(ret->num_columns * sizeof(DataRowParm));

	cur_pointer = ret->data;
	for (short int i = 0; i < ret->num_columns; i++) {
		ret->parms[i].length = ntohl(*(unsigned int*)cur_pointer);
		cur_pointer += sizeof(unsigned int);
		ret->parms[i].value = (char*)cur_pointer;
		cur_pointer += ret->parms[i].length;
	}

	return ret;
}
