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


BindComplete *make_bind_complete() {
	BindComplete *ret;

	ret = (BindComplete*)malloc(sizeof(BindComplete));
	memset(ret, 0, sizeof(BindComplete));

	ret->type = PSQL_BindComplete;
	ret->length = htonl(sizeof(uint32_t));

	return ret;
}
