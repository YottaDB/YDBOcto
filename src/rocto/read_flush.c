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

Flush *read_flush(BaseMessage *message, ErrorResponse **err) {
	Flush *ret;
	ErrorBuffer err_buff;
	const char *error_message;
	err_buff.offset = 0;


	if(message->type != PSQL_Flush) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Flush", message->type, PSQL_Flush);
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		return NULL;
	}

	ret = (Flush*)malloc(sizeof(Flush));
	ret->type = PSQL_Flush;
	ret->length = sizeof(unsigned int);

	return ret;
}
