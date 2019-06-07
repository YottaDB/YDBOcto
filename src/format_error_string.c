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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <stdlib.h>

#include <libyottadb.h>

#include "octo.h"
#include "errors.h"

// Creates a formatted string from variable argument input for use in error messages
const char *format_error_string(struct ErrorBuffer *err_buff, enum ERROR error, ...) {
	va_list args;
	va_start(args, error);
	int length = 0;
	unsigned int max_length = 0;
	char *ret = NULL;

	// Prevent overflow
	max_length = MAX_STR_CONST - err_buff->offset;
	ret = &err_buff->buffer[err_buff->offset];
	length = vsnprintf(ret, max_length, err_format_str[error], args);

	// Prevent subsequent overflow
	if (length + err_buff->offset >= MAX_STR_CONST) {
		WARNING(ERR_INVALID_WRITE_SIZE, length + err_buff->offset);
		return ret;
	}

	err_buff->offset += length;
	va_end(args);
	return ret;
}
