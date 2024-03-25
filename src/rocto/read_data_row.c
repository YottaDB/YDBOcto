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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

DataRow *read_data_row(BaseMessage *message) {
	DataRow *ret;
	char	*cur_pointer, *c;
	uint32_t remaining_length = 0;

	remaining_length = ntohl(message->length);
	ret = (DataRow *)malloc(remaining_length + sizeof(DataRow) - sizeof(uint32_t) - sizeof(int16_t));
	// Exclude DataRowParm array pointer
	memset(&ret->type, 0, remaining_length + sizeof(DataRow) - sizeof(uint32_t) - sizeof(int16_t) - sizeof(DataRowParm *));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(uint32_t);
	c = message->data;
	ret->num_columns = ntohs(*(int16_t *)c);
	if (ret->num_columns == 0) {
		ret->parms = NULL;
		return ret;
	}

	remaining_length -= sizeof(int16_t);
	c += sizeof(int16_t);
	memcpy(ret->data, c, remaining_length);
	ret->parms = (DataRowParm *)malloc(ret->num_columns * sizeof(DataRowParm));

	cur_pointer = ret->data;
	for (int16_t i = 0; i < ret->num_columns; i++) {
		ret->parms[i].length = ntohl(*(uint32_t *)cur_pointer);
		cur_pointer += sizeof(uint32_t);
		ret->parms[i].value = (char *)cur_pointer;
		cur_pointer += ret->parms[i].length;
	}

	return ret;
}
