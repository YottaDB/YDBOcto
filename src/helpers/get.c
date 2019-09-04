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

ydb_buffer_t *get(char *global, size_t num_args, ...) {
	va_list args;
	ydb_buffer_t *ret, *buffers;
	int status;

	va_start(args, num_args);
	buffers = vmake_buffers(global, num_args, args);
	va_end(args);

	ret = (ydb_buffer_t*)malloc(sizeof(ydb_buffer_t));
	YDB_MALLOC_BUFFER(ret, MAX_STR_CONST);

	status = ydb_get_s(&buffers[0], num_args, &buffers[1], ret);
	if(status == YDB_ERR_GVUNDEF || status == YDB_ERR_LVUNDEF) {
		YDB_FREE_BUFFER(ret);
		free(ret);
		free(buffers);
		return NULL;
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(ret);
		free(ret);
		free(buffers);
		return NULL;
	}

	free(buffers);
	return ret;
}
