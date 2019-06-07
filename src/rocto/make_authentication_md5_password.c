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

AuthenticationMD5Password *make_authentication_md5_password() {
	AuthenticationMD5Password *ret;

	ret = (AuthenticationMD5Password*)malloc(sizeof(AuthenticationMD5Password));
	memset(ret, 0, sizeof(AuthenticationMD5Password));

	ret->type = PSQL_AuthenticationMD5Password;
	ret->length = htonl(12);
	ret->md5_required = htonl(5);
	/// TODO: this should be a random number
	ret->salt[0] = 42;
	ret->salt[1] = 42;
	ret->salt[2] = 42;
	ret->salt[3] = 42;

	return ret;
}
