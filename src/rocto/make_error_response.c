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

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "errors.h"
#include "message_formats.h"

ErrorResponse *make_error_response(PSQL_ErrorSeverity severity, PSQL_SQLSTATECode code, const char *message, size_t num_args, ...) {
	assert(NULL != message);
	assert(PSQL_Error_UnknownSeverity >= severity);

	uint32_t	  new_length;
	int32_t		  cur_arg;
	va_list		  args;
	ErrorResponseArg *arg;
	ErrorResponse *	  ret;
	char *		  ptr;

	// Go through all the args and count their size
	new_length = 0;
	va_start(args, num_args);
	for (size_t i = 0; i < num_args; i++) {
		arg = va_arg(args, ErrorResponseArg *);
		// type argument
		new_length += 1;
		// string value; null terminated
		new_length += strlen(arg->value) + 1;
	}
	va_end(args);

	// Add the length of our error string, plus null byte and type
	new_length += strlen(psql_error_severity_str[severity]) + 2;
	// Length of the error code, plus null byte and type
	new_length += strlen(psql_sqlstate_codes_str[code]) + 2;
	// The first required message, plus null byte and type
	new_length += strlen(message) + 2;

	// Allocate struct + trailing array + null terminating byte
	new_length += 1;
	ret = (ErrorResponse *)malloc(sizeof(ErrorResponse) + new_length);
	memset(ret, 0, sizeof(ErrorResponse) + new_length);

	// This is mostly for testing; eventually, we should
	//  check whether this is a debug build, and if so, not do this maloc
	//  or the sets
	ret->args = (ErrorResponseArg *)malloc(sizeof(ErrorResponseArg) * (num_args + 3));
	cur_arg = 0;

	// Add length field
	// Depending on severity, select ErrorResponse or NoticeResponse
	switch (severity) {
	case PSQL_Error_LOG:
	case PSQL_Error_INFO:
	case PSQL_Error_DEBUG:
	case PSQL_Error_NOTICE:
	case PSQL_Error_WARNING:
		ret->type = PSQL_NoticeResponse;
		break;
	default:
		ret->type = PSQL_ErrorResponse;
		break;
	}
	ret->length = htonl(new_length + sizeof(uint32_t));
	ptr = ret->data;

	// Copy severity parameter
	ret->args[cur_arg].type = PSQL_Error_SEVERITY;
	ret->args[cur_arg].value = ptr;
	cur_arg++;
	*ptr++ = PSQL_Error_SEVERITY;
	int32_t i = strlen(psql_error_severity_str[severity]);
	memcpy(ptr, psql_error_severity_str[severity], i);
	ptr += i;
	*ptr++ = '\0';

	// Copy error code parameter
	ret->args[cur_arg].type = PSQL_Error_Code;
	ret->args[cur_arg].value = ptr;
	cur_arg++;
	*ptr++ = PSQL_Error_Code;
	i = strlen(psql_sqlstate_codes_str[code]);
	memcpy(ptr, psql_sqlstate_codes_str[code], i);
	ptr += i;
	*ptr++ = '\0';

	// Copy error message parameter
	ret->args[cur_arg].type = PSQL_Error_Message;
	ret->args[cur_arg].value = ptr;
	cur_arg++;
	*ptr++ = PSQL_Error_Message;
	i = strlen(message);
	memcpy(ptr, message, i);
	ptr += i;
	*ptr++ = '\0';

	// Copy optional arguments into argument array
	va_start(args, num_args);
	for (size_t i = 0; i < num_args; i++) {
		arg = va_arg(args, ErrorResponseArg *);
		*ptr++ = arg->type;
		ret->args[cur_arg].type = arg->type;
		ret->args[cur_arg].value = ptr;
		cur_arg++;
		strcpy(ptr, arg->value);
		ptr += strlen(arg->value);
		*ptr++ = '\0';
	}
	va_end(args);

	// Terminating symbol
	*ptr++ = '\0';
	return ret;
}
