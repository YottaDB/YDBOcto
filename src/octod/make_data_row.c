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


DataRow *make_data_row(DataRowParm *parms, short num_parms) {
	DataRow *ret;
	unsigned int length;
	char *c;
	int i;

	// Get the length we need to malloc
	length = 0;
	for(i = 0; i < num_parms; i++) {
		length += parms[i].length;
		length += sizeof(unsigned int);
	}

	ret = (DataRow*)malloc(sizeof(DataRow) + length);
	// Include the length of the length field
	length += sizeof(unsigned int);
	// Include the length of the num_parms field
	length += sizeof(short);
	ret->type = PSQL_DataRow;
	ret->length = htonl(length);
	ret->num_columns = htons(num_parms);

	c = ret->data;
	for(i = 0; i < num_parms; i++) {
		*((unsigned int*)c) = htonl(parms[i].length);
		c += sizeof(unsigned int);
		memcpy(c, parms[i].value, parms[i].length);
		c += parms[i].length;
	}

	return ret;
}
