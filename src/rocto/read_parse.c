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

Parse *read_parse(BaseMessage *message, ErrorResponse **err) {
	Parse *ret;
	char *cur_pointer, *last_byte;
	unsigned int remaining_length, i;
	// Begin Parse initialization from message
	remaining_length = ntohl(message->length);
	ret = (Parse*)malloc(remaining_length + sizeof(Parse));
	memset(ret, 0, remaining_length + sizeof(Parse));
	memcpy(&ret->type, message, remaining_length + 1);

	// Ensure correct message type
	if(ret->type != PSQL_Parse) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "Parse has incorrect message type",
					   0);
		free(ret);
		return NULL;
	}

	// Continue initialization
	remaining_length -= 4;
	ret->dest = ret->data;
	// Utility pointers
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;

	// Ensure destination null terminated
	while(cur_pointer < last_byte && *cur_pointer != '\0') {
		cur_pointer++;
	}
	if(cur_pointer == last_byte || *cur_pointer != '\0') {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "Parse destination missing null termination",
					   0);
		free(ret);
		return NULL;
	}
	// Ensure both dest and query fields included
	if(cur_pointer + 1 == last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "Parse missing destination or query field",
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;
	ret->query = cur_pointer;
	// Ensure query null terminated
	while(*cur_pointer != '\0' && cur_pointer < last_byte) {
		cur_pointer++;
	}
	if(*cur_pointer != '\0' || cur_pointer == last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "Parse query missing null termination",
					   0);
		free(ret);
		return NULL;
	}
	// Ensure number of parameters included
	if(cur_pointer + 1 == last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "Parse number of parameters missing",
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;
	ret->num_parms = ntohs(*((short *)(cur_pointer)));
	cur_pointer += sizeof(short);
	// Ensure parameter data types in bounds
	if(cur_pointer + sizeof(unsigned int) * ret->num_parms < last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
				PSQL_Code_Protocol_Violation,
				"Parse parameter data type list too long",
				0);
		free(ret);
		return NULL;
	}
	// Ensure all parameter data types included
	if(cur_pointer + sizeof(unsigned int) * ret->num_parms > last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
				PSQL_Code_Protocol_Violation,
				"Parse parameter data type list too short",
				0);
		free(ret);
		return NULL;
	}
	// We could malloc a new array to store the values converted from network endian here,
	//  but that makes cleanup a bit more messy. Given that there is no reason for a backend
	//  send a read_parse message, just alter it in place. If we ever sent it, we would need
	//  to go back through and convert back to network endian
	ret->parm_data_types = (unsigned int *)cur_pointer;
	for(i = 0; i < ret->num_parms; i++) {
		ret->parm_data_types[i] = ntohl(*((int*)(&ret->parm_data_types[i])));
		cur_pointer += sizeof(unsigned int);
	}
	return ret;
}
