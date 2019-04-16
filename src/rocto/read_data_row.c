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

DataRow *read_data_row(BaseMessage *message, ErrorResponse **err) {
	DataRow *ret;
	char *cur_pointer, *last_byte, *c;
	unsigned int remaining_length = 0, i = 0;

	remaining_length = ntohl(message->length);
	ret = (DataRow*)malloc(remaining_length + sizeof(DataRow) - sizeof(unsigned int) - sizeof(short int));
	// Exclude DataRowParm array pointer
	memset(&ret->type, 0, remaining_length + sizeof(DataRow) - sizeof(unsigned int) - sizeof(short int) - sizeof(DataRowParm*));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(unsigned int);
	c = message->data;
	ret->num_columns = ntohs(*(short*)c);
	if (ret->num_columns == 0) {
		ret->parms = NULL;
		return ret;
	}

	remaining_length -= sizeof(short int);
	c += sizeof(short int);
	memcpy(ret->data, c, remaining_length);
	ret->parms = (DataRowParm*)malloc(ret->num_columns * sizeof(DataRowParm));

	cur_pointer = ret->data;
	for (i = 0; i < ret->num_columns; i++) {
		ret->parms[i].length = ntohl(*(unsigned int*)cur_pointer);
		cur_pointer += sizeof(unsigned int);
		ret->parms[i].value = (char*)cur_pointer;
		cur_pointer += ret->parms[i].length;
	}

	return ret;
}
