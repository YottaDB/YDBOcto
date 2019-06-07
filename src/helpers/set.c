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

#include <string.h>

#include "octo.h"

#include "helpers.h"

void set(char *new_value, char *global, size_t num_args, ...) {
	va_list args;
	ydb_buffer_t *ret, *buffers;
	ydb_buffer_t z_status, z_status_value;
	int status;

	va_start(args, num_args);
	buffers = vmake_buffers(global, num_args, args);
	va_end(args);

	ret = (ydb_buffer_t*)malloc(sizeof(ydb_buffer_t));
	YDB_MALLOC_BUFFER(ret, MAX_STR_CONST);
	YDB_COPY_STRING_TO_BUFFER(new_value, ret, status);

	status = ydb_set_s(&buffers[0], num_args, &buffers[1], ret);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	YDB_FREE_BUFFER(ret);
	free(buffers);
	free(ret);
}
