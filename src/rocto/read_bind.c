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

Bind *read_bind(BaseMessage *message, ErrorResponse **err) {
	Bind *ret;
	char *cur_pointer, *last_byte, *length_ptr;
	unsigned int remaining_length;
	int i = 0;

	// We must be sure ALL bytes are initialized, otherwise we could have
	//  unpredictability or leak private data to clients when we respond
	remaining_length = ntohl(message->length);
	ret = (Bind*)malloc(remaining_length + sizeof(Bind));
	memset(ret, 0, remaining_length + sizeof(Bind));
	// Since the length does not include the type, we need one more byte to be copied
	memcpy(&ret->type, message, remaining_length + 1);
	// The data section doesn't include the length or format code
	remaining_length -= 4;
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;
	ret->dest = cur_pointer;
	while(*cur_pointer != '\0' && cur_pointer < last_byte)
		cur_pointer++;
	if(*cur_pointer != '\0' || cur_pointer == last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "bind destination missing null termination",
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;
	ret->source = cur_pointer;
	while(*cur_pointer != '\0' && cur_pointer < last_byte)
		cur_pointer++;
	if(*cur_pointer != '\0' || cur_pointer == last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "bind SQL missing null termination",
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;
	ret->num_parm_format_codes = ntohs(*((short int *)cur_pointer));
	cur_pointer += sizeof(short int);
	if(ret->num_parm_format_codes > 0)
		ret->parm_format_codes = (void*)cur_pointer;
	cur_pointer += ret->num_parm_format_codes * sizeof(short int);
	if(cur_pointer > last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "bind incomplete/missing parameter format code list",
					   0);
		free(ret);
		return NULL;
	}
	ret->num_parms = ntohs(*((short int*)cur_pointer));
	cur_pointer += sizeof(short int);
	if(cur_pointer > last_byte) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "bind incomplete/missing parameters",
					   0);
		free(ret->parms);
		free(ret);
		return NULL;
	}
	// We don't know how long each parameter is, so advance the
	//  pointer by hand past each parameter, and record pointers to them so the handler knows
	//  where they are
	if(ret->num_parms > 0) {
		ret->parms = (BindParm*)malloc(sizeof(BindParm) * ret->num_parms);
		memset(ret->parms, 0, sizeof(BindParm) * ret->num_parms);
		for(i = 0; i < ret->num_parms; i++) {
			// This length does not include the length value; go past that too
			ret->parms[i].value = cur_pointer + 4;
			length_ptr = cur_pointer;
			cur_pointer += 4;
			if(cur_pointer > last_byte) {
				*err = make_error_response(PSQL_Error_ERROR,
							   PSQL_Code_Protocol_Violation,
							   "bind incomplete/missing parameters",
							   0);
				free(ret->parms);
				free(ret);
				return NULL;
			}
			ret->parms[i].length = *((long int*)(length_ptr));
			cur_pointer += ret->parms[i].length;
			if(cur_pointer > last_byte) {
				*err = make_error_response(PSQL_Error_ERROR,
							   PSQL_Code_Protocol_Violation,
							   "bind incomplete/missing parameters",
							   0);
				free(ret->parms);
				free(ret);
				return NULL;
			}
		}
	}
	ret->num_result_col_format_codes = ntohs(*((short int*)cur_pointer));
	cur_pointer += sizeof(short int);
	if(ret->num_result_col_format_codes > 0) {
		ret->result_col_format_codes = (void*)cur_pointer;
		cur_pointer += ret->num_result_col_format_codes * sizeof(short int);
		if(cur_pointer > last_byte) {
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   "bind missing/incomplete result format codes",
						   0);
			free(ret->parms);
			free(ret);
			return NULL;
		}
	}

	// Should be good to go; verify we used everything
	if(cur_pointer != last_byte) {
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   "bind message has trailing characters",
					   0);
	}

	return ret;
}
