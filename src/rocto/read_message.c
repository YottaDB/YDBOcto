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

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

BaseMessage *read_message(RoctoSession *session, char *buffer, int buffer_size) {
	BaseMessage *message;
	int result;

	// Read BaseMessage fields (type, length)
	message = (void*)buffer;
	result = read_bytes(session, (char*)message, buffer_size, sizeof(char) + sizeof(unsigned int));
	if(result == 1)
		return NULL;

	// Read the rest of the message
	result = read_bytes(session, (char*)(&message->data), buffer_size, ntohl(message->length) - sizeof(unsigned int));
	if(result == 1)
		return NULL;

	return message;
}
