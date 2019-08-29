/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"


RowDescription *make_row_description(RowDescriptionParm *parms, int16_t num_parms) {
	RowDescription *ret;
	uint32_t length = 0, cur_str_length;
	char *c;
	int32_t i;

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
	length += sizeof(uint32_t);
	// Count the num_parms field
	length += sizeof(int16_t);

	ret->type = PSQL_RowDescription;
	ret->length = htonl(length);
	ret->num_parms = htons(num_parms);
	ret->parms = NULL;

	// Copy in each parm
	c = ret->data;
	for(i = 0; i < num_parms; i++) {
		// Copy string
		cur_str_length = strlen(parms[i].name);
		memcpy(c, parms[i].name, cur_str_length);
		c += cur_str_length;
		*c++ = '\0';

		// Copy values, converting them to network endianess
		*((int*)c) = htonl(parms[i].table_id);
		c += sizeof(int);
		*((int16_t*)c) = htons(parms[i].column_id);
		c += sizeof(int16_t);
		*((int*)c) = htonl(parms[i].data_type);
		c += sizeof(int);
		*((int16_t*)c) = htons(parms[i].data_type_size);
		c += sizeof(int16_t);
		*((int*)c) = htonl(parms[i].type_modifier);
		c += sizeof(int);
		*((int16_t*)c) = htons(parms[i].format_code);
		c += sizeof(int16_t);
	}

	return ret;
}
