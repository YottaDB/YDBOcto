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
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

PasswordMessage *read_password_message(BaseMessage *message, ErrorResponse **err) {
	PasswordMessage *ret;
	ErrorBuffer err_buff;
	unsigned int length;
	char *c, *message_end;
	const char *error_message;
	err_buff.offset = 0;

	length = ntohl(message->length);
	ret = (PasswordMessage*)malloc(sizeof(PasswordMessage) + length - sizeof(unsigned int));
	ret->type = message->type;
	ret->length = length;
	memcpy(ret->data, message->data, length - sizeof(unsigned int));
	c = ret->data;
	message_end = c + length - sizeof(unsigned int);

	// Ensure that message has correct type
	if(ret->type != PSQL_PasswordMessage) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "PasswordMessage", ret->type, PSQL_PasswordMessage);
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	// Find end of password string
	while(c < message_end && *c != '\0') {
		c++;
	}
	if(c == message_end) {
		// Ensure a password string is included
		if(length == sizeof(unsigned int)) {
			error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "PasswordMessage", "password");
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Syntax_Error,
						   error_message,
						   0);
			free(ret);
			return NULL;
		}
		// Ensure password has null terminator
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "PasswordMessage", "password");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	else if(c < message_end - 1) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "PasswordMessage");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	ret->password = ret->data;

	return ret;
}
