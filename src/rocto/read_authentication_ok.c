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

AuthenticationOk *read_authentication_ok(BaseMessage *message, ErrorResponse **err) {
	AuthenticationOk *ret = NULL;
	unsigned int remaining_length = 0;

	remaining_length = ntohl(message->length);
	ret = (AuthenticationOk*)malloc(sizeof(AuthenticationOk));

	ret->type = message->type;
	ret->length = message->length;
	memcpy(&ret->result, &message->data, sizeof(int));

	return ret;
}
