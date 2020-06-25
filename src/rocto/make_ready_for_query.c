/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

ReadyForQuery *make_ready_for_query(PSQL_TransactionStatus status) {
	ReadyForQuery *ret;

	ret = (ReadyForQuery *)malloc(sizeof(ReadyForQuery));
	memset(ret, 0, sizeof(ReadyForQuery));

	ret->type = PSQL_ReadyForQuery;
	ret->length = htonl(sizeof(uint32_t) + sizeof(char));
	ret->status = status;

	return ret;
}
