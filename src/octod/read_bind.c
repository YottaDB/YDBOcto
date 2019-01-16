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

#include "message_formats.h"

Bind *read_bind(BaseMessage *message) {
	Bind *ret;
	char *cur_pointer, *last_byte;
	size_t remaining_length;
	int i = 0;

	// We must be sure ALL bytes are initialized, otherwise we could have
	//  unpredictability or leak private data to clients when we respond
	remaining_length = ntohl(message->length);
	ret = (Bind*)malloc(remaining_length + sizeof(Bind));
	memset(ret, 0, remaining_length + sizeof(Bind));
	memcpy(&ret->type, message, remaining_length);
	// The data section doesn't include the length or format code
	remaining_length -= 5;
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;
	ret->dest = cur_pointer;
	while(*cur_pointer != '\0' && cur_pointer < last_byte)
		cur_pointer++;
	if(*cur_pointer != '\0') {
		// Error handling should happen here
		assert(0);
	}
	cur_pointer++;
	ret->source = cur_pointer;
	while(*cur_pointer != '\0' && cur_pointer < last_byte)
		cur_pointer++;
	if(*cur_pointer != '\0') {
		// Error handling should happen here
		assert(0);
	}
	cur_pointer++;
	ret->num_parm_format_codes = ntohs(*cur_pointer);
	cur_pointer += sizeof(short int);
	if(ret->num_parm_format_codes > 0)
		ret->parm_format_codes = (void*)cur_pointer;
	cur_pointer += ret->num_parm_format_codes;
	if(cur_pointer > last_byte) {
		assert(0);
	}
	ret->num_parms = ntohs(*cur_pointer);
	cur_pointer += sizeof(short int);
	// We don't know how long each parameter is, so advance the
	//  pointer by hand past each parameter
	if(ret->num_parms > 0) {
		ret->parms = (BindParm*)malloc(sizeof(BindParm) * ret->num_parms);
		memset(ret->parms, 0, sizeof(BindParm) * ret->num_parms);
		for(i = 0; i < ret->num_parms; i++) {
			// This length does not include the length value; go past that too
			ret->parms[i].value = cur_pointer + 4;
			ret->parms[i].length = *((long int*)(cur_pointer));
			cur_pointer += ret->parms[i].length + 4;
			if(cur_pointer > last_byte) {
				assert(0);
			}
		}
	}
	ret->num_result_col_format_codes = ntohs(*cur_pointer);
	if(ret->result_col_format_codes > 0) {
		cur_pointer += sizeof(short int);
		ret->result_col_format_codes = (void*)cur_pointer;
	}

	// Should be good to go; verify we used everything
	if(cur_pointer != last_byte) {
		assert(0);
	}

	return ret;
}
