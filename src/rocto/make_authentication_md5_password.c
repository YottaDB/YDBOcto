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

// Used for random number generation via syscall
#include <sys/syscall.h>

#include "rocto.h"
#include "helpers.h"
#include "message_formats.h"

AuthenticationMD5Password *make_authentication_md5_password(RoctoSession *session, char *salt) {
	if (NULL == session) {
		return NULL;
	}
	AuthenticationMD5Password *ret;

	ret = (AuthenticationMD5Password*)malloc(sizeof(AuthenticationMD5Password));
	memset(ret, 0, sizeof(AuthenticationMD5Password));

	ret->type = PSQL_AuthenticationMD5Password;
	ret->length = htonl(sizeof(int) + sizeof(int) + sizeof(char) * 4);
	ret->md5_required = htonl(5);
	// Generate 4-byte random salt with call to getrandom syscall (#318)
	syscall(318, salt, 4, 0);
	memcpy(ret->salt, salt, 4);

	return ret;
}
