/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

AuthenticationOk *make_authentication_ok(void) {
	AuthenticationOk *ret;

	ret = (AuthenticationOk *)malloc(sizeof(AuthenticationOk));
	memset(ret, 0, sizeof(AuthenticationOk));

	ret->type = PSQL_AuthenticationOk;
	ret->length = htonl(8);
	ret->result = htonl(0);

	return ret;
}
