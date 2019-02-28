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

Query *read_query(BaseMessage *message, ErrorResponse **err) {
	Query *ret;
	int length;
	char *c, *message_end;
	
	assert(message->type == PSQL_Query);

	length = ntohl(message->length);
	ret = (Query*)malloc(sizeof(Query) + length - 4);
	ret->type = message->type;
	ret->length = length;
	memcpy(ret->data, message->data, length - 4);
	c = ret->data;
	message_end = c + length - 4;

	// Ensure that there is a trailing null character
	for(; c < message_end && *c != '\0'; c++) {
		// Left blank
	}
	if(*c != '\0') {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   "No null terminating character on input",
					   0);
		free(ret);
		return NULL;
	}
	ret->query = ret->data;

	return ret;
}
