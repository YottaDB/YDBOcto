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
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

Flush *read_flush(BaseMessage *message) {
	Flush *	 ret;
	uint32_t expected_length = sizeof(uint32_t);

	if (message->type != PSQL_Flush) {
		ERROR(ERR_ROCTO_INVALID_TYPE, "Flush", message->type, PSQL_Flush);
		return NULL;
	}

	ret = (Flush *)malloc(sizeof(Flush));
	ret->type = PSQL_Flush;
	ret->length = ntohl(message->length);
	// Length must be 4 (one int)
	if (ret->length != expected_length) {
		ERROR(ERR_ROCTO_INVALID_INT_VALUE, "Flush", "length", ret->length, expected_length);
		free(ret);
		return NULL;
	}

	return ret;
}
