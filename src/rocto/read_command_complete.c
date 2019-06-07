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

CommandComplete *read_command_complete(BaseMessage *message, ErrorResponse **err) {
	CommandComplete *ret;
	char *cur_pointer, *last_byte;
	unsigned int remaining_length = 0;

	remaining_length = ntohl(message->length);
	ret = (CommandComplete*)malloc(remaining_length + sizeof(CommandComplete) - sizeof(unsigned int));

	ret->type = message->type;
	ret->length = remaining_length;
	memcpy(ret->data, message->data, remaining_length - sizeof(unsigned int));
	ret->command_tag = ret->data;

	return ret;
}
