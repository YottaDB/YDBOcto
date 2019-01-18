/* Copyright (C) 2018-2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"

ErrorResponse *make_error_response(PSQL_ErrorSeverity severity, PSQL_SQLSTATECode code, char *message, size_t num_args, ...) {
	unsigned int new_length;
	int i;
	va_list args;
	ErrorResponseArg *arg;
	ErrorResponse *ret;
	char *ptr;

	// Go through all the args and count their size
	new_length = 0;
	va_start(args, num_args);
	for(i = 0; i < num_args; i++) {
		arg = va_arg(args, ErrorResponseArg*);
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
	ret = (ErrorResponse*)malloc(sizeof(ErrorResponse) + new_length);
	memset(ret, 0, sizeof(ErrorResponse) + new_length);
	ret->type = PSQL_ErrorResponse;
	// Add 4 length field
	ret->length = htonl(new_length + 4);
	ptr = ret->data;

	// Copy first three parameters
	*ptr++ = PSQL_Error_SEVERITY;
	i = strlen(psql_error_severity_str[severity]);
	memcpy(ptr, psql_error_severity_str[severity], i);
	ptr += i;
	*ptr++ = '\0';

	*ptr++ = PSQL_Error_Code;
	i = strlen(psql_sqlstate_codes_str[code]);
	memcpy(ptr, psql_sqlstate_codes_str[code], i);
	ptr += i;
	*ptr++ = '\0';

	*ptr++ = PSQL_Error_Message;
	i = strlen(message);
	memcpy(ptr, message, i);
	ptr += i;
	*ptr++ = '\0';

	// Copy values to the array
	ret->args = (ErrorResponseArg*)malloc(sizeof(ErrorResponseArg) * num_args);
	va_start(args, num_args);
	for(i = 0; i < num_args; i++) {
		arg = va_arg(args, ErrorResponseArg*);
		*ptr++ = arg->type;
		ret->args[i].type = arg->type;
		ret->args[i].value = ptr;
		strcpy(ptr, arg->value);
		ptr += strlen(arg->value);
		*ptr++ = '\0';
	}
	va_end(args);

	// Terminating symbol
	*ptr++ = '\0';
	return ret;
}
