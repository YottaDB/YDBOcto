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

#include <errno.h>

#include <libyottadb.h>

#include "octo.h"

int64_t create_cursor(ydb_buffer_t *schema_global, ydb_buffer_t *cursor_buffer) {
	int status = 0;
	int64_t cursorId;

	status = ydb_incr_s(schema_global, 0, NULL, NULL, cursor_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return -1;
	}
	cursor_buffer->buf_addr[cursor_buffer->len_used] = '\0';

	cursorId = strtol(cursor_buffer->buf_addr, NULL, 10);
	if ((LONG_MAX != cursorId) && (LONG_MIN != cursorId) && (0 <= cursorId)) {
		return cursorId;
	} else {
		ERROR(ERR_LIBCALL, "strtol")
		return -1;
	}
}
