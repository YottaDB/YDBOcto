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
	char *cur_pointer, *last_byte, *length_ptr;
	unsigned int remaining_length;
	int i = 0;

	// We must be sure ALL bytes are initialized, otherwise we could have
	//  unpredictability or leak private data to clients when we respond
	remaining_length = ntohl(message->length);
	ret = (Describe*)malloc(remaining_length + sizeof(Describe));
	memset(ret, 0, remaining_length + sizeof(Describe));
	// Since the length does not include the type, we need one more byte to be copied
	memcpy(&ret->type, message, remaining_length + 1);
	// The data section doesn't include the length or format code
	remaining_length -= 4;
	remaining_length -= 1; // Remove flag
	// Ensure we use the entire message, that it's not malformed
	cur_pointer = ret->name;
	last_byte = cur_pointer + remaining_length;
	while(*cur_pointer != '\0')
		cur_pointer++;
	cur_pointer++;
	// Should be good to go; verify we used everything
	if(cur_pointer != last_byte) {
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   "describe message has trailing characters",
					   0);
		return NULL;
	}

	return ret;
}
