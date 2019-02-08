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

#include "message_formats.h"


RowDescription *make_row_description(RowDescriptionParm *parms, short num_parms) {
	RowDescription *ret;
	unsigned int length = 0, cur_str_length;
	char *c;
	int i;

	// Get a count of the needed length
	for(i = 0; i < num_parms; i++) {
		// Name and null
		length += strlen(parms[i].name) + 1;
		// The other elements
		length += sizeof(RowDescriptionParm) - sizeof(char*);
	}

	ret = (RowDescription*)malloc(sizeof(RowDescription) + length);
	memset(ret, 0, sizeof(RowDescription) + length);
	// Count the length field as part of the length
	length += sizeof(unsigned int);
	// Count the num_parms field
	length += sizeof(short);

	ret->type = PSQL_RowDescription;
	ret->length = htonl(length);
	ret->num_parms = htons(num_parms);

	// Copy in each parm
	c = ret->data;
	for(i = 0; i < num_parms; i++) {
		// Copy string
		cur_str_length = strlen(parms[i].name);
		memcpy(ret, parms[i].name, cur_str_length);
		c += cur_str_length;
		*c++ = '\0';

		// Copy values, converting them to network endianess
		*((int*)c) = htonl(parms[i].table_id);
		c += sizeof(int);
		*((short*)c) = htons(parms[i].column_id);
		c += sizeof(short);
		*((int*)c) = htonl(parms[i].data_type);
		c += sizeof(int);
		*((short*)c) = htons(parms[i].data_type_size);
		c += sizeof(short);
		*((int*)c) = htonl(parms[i].type_modifier);
		c += sizeof(int);
		*((short*)c) = htons(parms[i].format_code);
		c += sizeof(short);
	}

	return ret;
}
