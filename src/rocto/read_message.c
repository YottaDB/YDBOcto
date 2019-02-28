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

#include "rocto.h"
#include "message_formats.h"

BaseMessage *read_message(RoctoSession *session, char *buffer, int buffer_size) {
	BaseMessage *message;
	int result;

	message = (void*)buffer;
	result = read_bytes(session, (char*)message, buffer_size, 5);
	if(result == 1)
		return NULL;

	// Read the rest of the message
	read_bytes(session, (char*)(&message->data), buffer_size, ntohl(message->length) - 4);
	if(result == 1)
		return NULL;

	return message;
}
