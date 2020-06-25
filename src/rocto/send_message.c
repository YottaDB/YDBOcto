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

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include <openssl/ssl.h>
#include <openssl/err.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

int send_message(RoctoSession *session, BaseMessage *message) {
	int32_t result = 0;

	TRACE(ERR_ENTERING_FUNCTION, "send_message");
	TRACE(ERR_SEND_MESSAGE, message->type, ntohl(message->length));

	// +1 for message type indicator
	result = send_bytes(session, (char *)message, ntohl(message->length) + 1);
	if (0 != result)
		ERROR(ERR_ROCTO_SEND_MESSAGE, message->type);
	return result;
}
