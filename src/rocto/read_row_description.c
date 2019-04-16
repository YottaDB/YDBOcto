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

RowDescription *read_row_description(BaseMessage *message, ErrorResponse **err) {
	RowDescription *ret;
	char *cur_pointer, *last_byte;
	unsigned int remaining_length = 0, i = 0, cur_length = 0;

	remaining_length = ntohl(message->length);
	ret = (RowDescription*)malloc(remaining_length + sizeof(RowDescription) - sizeof(unsigned int) - sizeof(short int));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(unsigned int);
	cur_pointer = message->data;

	ret->num_parms = ntohs(*(short int*)cur_pointer);
	remaining_length -= sizeof(short int);
	cur_pointer += sizeof(short int);

	memcpy(ret->data, cur_pointer, remaining_length);
	cur_pointer = ret->data;

	ret->parms = (RowDescriptionParm*)malloc(ret->num_parms * sizeof(RowDescriptionParm));
	for(i = 0; i < ret->num_parms; i++) {
		ret->parms[i].name = cur_pointer;
		cur_pointer += strlen(ret->parms[i].name);
		cur_pointer += sizeof(char);	// skip null

		// Copy values, converting them to host endianess
		ret->parms[i].table_id = ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].column_id = ntohs(*((short*)cur_pointer));
		cur_pointer += sizeof(short);
		ret->parms[i].data_type	= ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].data_type_size = ntohs(*((short*)cur_pointer));
		cur_pointer += sizeof(short);
		ret->parms[i].type_modifier = ntohl(*((int*)cur_pointer));
		cur_pointer += sizeof(int);
		ret->parms[i].format_code = ntohs(*((short*)cur_pointer));
		cur_pointer += sizeof(short);
	}

	return ret;
}
