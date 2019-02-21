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

#include "octod.h"
#include "message_formats.h"

Execute *read_execute(BaseMessage *message, ErrorResponse **err) {
	Execute *ret;
	char *cur_pointer, *last_byte;
	unsigned int remaining_length, i;

	remaining_length = ntohl(message->length);
	ret = (Execute*)malloc(remaining_length + sizeof(Execute));
	memset(ret, 0, remaining_length + sizeof(Execute));
	memcpy(&ret->type, message, remaining_length + 1);
	remaining_length -= 4;
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;

	ret->source = cur_pointer;
	while(*cur_pointer != '\0' && cur_pointer < last_byte) {
		cur_pointer++;
	}
	if(*cur_pointer != '\0' || cur_pointer == last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "execute destination missing null termination",
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;
	if(cur_pointer + 4 > last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "execute missing rows_to_returns",
					   0);
		free(ret);
		return NULL;
	}
	ret->rows_to_return = ntohl(*((unsigned int*)cur_pointer));

	return ret;
}
