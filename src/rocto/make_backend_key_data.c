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
#include <sys/syscall.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

BackendKeyData *make_backend_key_data(int secret_key, pid_t pid) {
	BackendKeyData *ret;

	ret = (BackendKeyData*)malloc(sizeof(BackendKeyData));
	memset(ret, 0, sizeof(BackendKeyData));

	// Populate message fields
	ret->type = PSQL_BackendKeyData;
	ret->length = htonl(sizeof(unsigned int) + sizeof(pid_t) + sizeof(int));
	ret->pid = htonl(pid);
	ret->secret_key = htonl(secret_key);

	return ret;
}
