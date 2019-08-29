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

Query *read_query(BaseMessage *message, ErrorResponse **err) {
	Query *ret;
	ErrorBuffer err_buff;
	uint32_t length;
	char *c, *message_end;
	const char *error_message;
	err_buff.offset = 0;

	length = ntohl(message->length);
	ret = (Query*)malloc(sizeof(Query) + length - sizeof(uint32_t));
	ret->type = message->type;
	ret->length = length;
	memcpy(ret->data, message->data, length - sizeof(uint32_t));
	c = ret->data;
	message_end = c + length - sizeof(uint32_t);

	// Ensure that message has correct type
	if(ret->type != PSQL_Query) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Query", ret->type, PSQL_Query);
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	// Find end of query string
	while(c < message_end && *c != '\0') {
		c++;
	}
	if(c == message_end) {
		// Ensure a query string is included
		if(length == sizeof(uint32_t)) {
			error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Query", "query");
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Syntax_Error,
						   error_message,
						   0);
			free(ret);
			return NULL;
		}
		// Ensure query has null terminator
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Query", "query");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	else if(c < message_end - 1) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "Query");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	ret->query = ret->data;

	return ret;
}
