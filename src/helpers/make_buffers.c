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

#include <string.h>

#include "octo.h"
#include "helpers.h"

// Makes an array of ydb_buffers (subscripts), beginning with global name and successively adding each name from args
ydb_buffer_t *make_buffers(char *global, size_t num_args, ...) {
	va_list	      args;
	ydb_buffer_t *ret;

	va_start(args, num_args);
	ret = vmake_buffers(global, num_args, args);
	va_end(args);

	return ret;
}

ydb_buffer_t *vmake_buffers(char *global, size_t num_args, va_list args) {
	ydb_buffer_t *buffers;
	char *	      arg;
	int	      i;

	buffers = (ydb_buffer_t *)malloc((num_args + 1) * sizeof(ydb_buffer_t));

	buffers[0].buf_addr = global;
	buffers[0].len_used = buffers[0].len_alloc = strlen(global);

	for (i = 0; i < num_args; i++) {
		arg = va_arg(args, char *);
		buffers[i + 1].buf_addr = arg;
		buffers[i + 1].len_alloc = buffers[i + 1].len_used = strlen(arg);
	}

	return buffers;
}
