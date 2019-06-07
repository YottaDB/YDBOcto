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
