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


ParameterStatus *make_parameter_status(StartupMessageParm *parm) {
	uint32_t length;
	ParameterStatus *ret;
	char *c;
	int32_t name_len, value_len;

	if (NULL == parm) {
		return NULL;
	}

	length = 0;
	length += sizeof(uint32_t);
	name_len = strlen(parm->name);
	length += name_len + 1;
	value_len = strlen(parm->value);
	length += value_len + 1;

	// malloc space for everything, but don't count length field twice
	ret = (ParameterStatus*)malloc(length + sizeof(ParameterStatus) - sizeof(uint32_t));
	memset(ret, 0, sizeof(ParseComplete));

	ret->type = PSQL_ParameterStatus;
	ret->length = htonl(length);
	c = ret->data;
	memcpy(c, parm->name, name_len);
	c += name_len;
	*c++ = '\0';
	memcpy(c, parm->value, value_len);
	c += value_len;
	*c++ = '\0';

	return ret;
}
