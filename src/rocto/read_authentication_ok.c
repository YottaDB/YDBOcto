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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

AuthenticationOk *read_authentication_ok(BaseMessage *message, ErrorResponse **err) {
	AuthenticationOk *ret = NULL;
	unsigned int remaining_length = 0;

	remaining_length = ntohl(message->length);
	ret = (AuthenticationOk*)malloc(sizeof(AuthenticationOk));

	ret->type = message->type;
	ret->length = message->length;
	memcpy(&ret->result, &message->data, sizeof(int));

	return ret;
}
