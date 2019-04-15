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

Describe *read_describe(BaseMessage *message, ErrorResponse **err) {
	Describe *ret;
	ErrorBuffer err_buff;
	char *cur_pointer, *last_byte;
	const char *error_message;
	unsigned int remaining_length;
	err_buff.offset = 0;

	// Create Describe struct and initialize ALL bytes to prevent leaks
	remaining_length = ntohl(message->length);
	ret = (Describe*)malloc(remaining_length + sizeof(Describe));
	memset(ret, 0, remaining_length + sizeof(Describe));

	// Copy entire message, including byte for the type field
	memcpy(&ret->type, message, remaining_length + 1);
	// The data section doesn't include the length or format code
	remaining_length -= sizeof(unsigned int);	// Exclude length
	remaining_length -= sizeof(char);		// Exclude format

	// Ensure valid value for type field
	if (ret->type != 'D') {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Describe", ret->type, PSQL_Describe);
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
                free(ret);
		return NULL;
	}
	// Ensure valid value for item field
	if (ret->item != 'S' && ret->item != 'P') {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_CHAR_VALUE, "Describe", "item",
				ret->item, "'S' or 'P'");
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
                free(ret);
		return NULL;
	}

	// Ensure we use the entire message
	cur_pointer = ret->name;
	last_byte = cur_pointer + remaining_length;
	while('\0' != *cur_pointer)
		cur_pointer++;
	cur_pointer++;
	// Check for missing null terminator
	if(cur_pointer > last_byte) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Describe", "name");
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
                free(ret);
		return NULL;
	}
	// Check for premature null terminator
	if(cur_pointer < last_byte) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "Describe");
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
                free(ret);
		return NULL;
	}

	return ret;
}
