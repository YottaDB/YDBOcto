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

ParameterStatus *read_parameter_status(BaseMessage *message, ErrorResponse **err) {
	ParameterStatus *ret;
	char *cur_pointer, *last_byte;
	unsigned int remaining_length = 0;

	if (NULL == message) {
		return NULL;
	}

	remaining_length = ntohl(message->length);
	ret = (ParameterStatus*)malloc(remaining_length + sizeof(ParameterStatus) - sizeof(unsigned int));

	ret->type = message->type;
	ret->length = remaining_length;
	memcpy(ret->data, message->data, remaining_length - sizeof(unsigned int));

	return ret;
}
