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
	unsigned int length;
	char *c, *message_end;

	length = ntohl(message->length);
	ret = (Query*)malloc(sizeof(Query) + length - sizeof(unsigned int));
	ret->type = message->type;
	ret->length = length;
	memcpy(ret->data, message->data, length - sizeof(unsigned int));
	c = ret->data;
	message_end = c + length - sizeof(unsigned int);

	// Ensure that message has correct type
	if(ret->type != PSQL_Query) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   "Query has incorrect type",
					   0);
		free(ret);
		return NULL;
	}
	// Find end of query string
	while(c < message_end && *c != '\0') {
		c++;
	}
	if(c == message_end) {
		// Ensure a query string is included
		if(length == sizeof(unsigned int)) {
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Syntax_Error,
						   "Query missing query string",
						   0);
			free(ret);
			return NULL;
		}
		// Ensure that there is a trailing null character
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   "No null terminating character on input",
					   0);
		free(ret);
		return NULL;
	}
	else if(c < message_end - 1) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   "Unexpected terminating character in input",
					   0);
		free(ret);
		return NULL;
	}
	ret->query = ret->data;

	return ret;
}
