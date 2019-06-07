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

Sync *read_sync(BaseMessage *message, ErrorResponse **err) {
	Sync *ret;
	ErrorBuffer err_buff;
	const char *error_message;
	err_buff.offset = 0;


	if(message->type != PSQL_Sync) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Sync", message->type, PSQL_Sync);
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		return NULL;
	}

	ret = (Sync*)malloc(sizeof(Sync));
	ret->type = PSQL_Sync;
	ret->length = sizeof(unsigned int);

	return ret;
}
