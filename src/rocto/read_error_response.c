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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

ErrorResponse *read_error_response(BaseMessage *message, ErrorResponse **err) {
	ErrorResponse *ret = NULL;
	char *cur_pointer = NULL, *last_byte = NULL;
	unsigned int remaining_length = 0, num_args = 0, i = 0;

	remaining_length = ntohl(message->length);
	ret = (ErrorResponse*)malloc(remaining_length + sizeof(ErrorResponse) - sizeof(unsigned int));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(unsigned int);
	memcpy(ret->data, message->data, remaining_length);
	cur_pointer = ret->data;
	last_byte = ret->data + remaining_length;

	// Count number of arguments
	while (cur_pointer < last_byte) {
		cur_pointer++;		// skip type indicator
		while (cur_pointer != last_byte && '\0' != *cur_pointer) {
			cur_pointer++;
		}
		cur_pointer++;		// skip null terminator
		num_args++;
	}

	ret->args = (ErrorResponseArg*)malloc(num_args * sizeof(ErrorResponseArg));

	// Populate args with type info and pointers into data section
	cur_pointer = ret->data;
	for (i = 0; i < num_args; i++) {
		ret->args[i].type = *cur_pointer;
		cur_pointer++;
		ret->args[i].value = cur_pointer;
		while (cur_pointer != last_byte && '\0' != *cur_pointer) {
			cur_pointer++;
		}
		cur_pointer++;		// skip null terminator
	}

	return ret;
}
