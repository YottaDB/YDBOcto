/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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
#include <stdint.h>
#include <string.h>
#include <errno.h>

#include "octo.h"

// Returns the amount of memory used by the current process
int64_t get_mem_usage() {
	FILE *fp;
	char command[MAX_STR_CONST];
	char mem_used[INT64_TO_STRING_MAX];
	int64_t ret;
	char *status;

	snprintf(command, MAX_STR_CONST, "ps -p %d -o vsize | cut -d ' ' -f 2 | tr -d '\\n'", getpid());

	fp = popen(command, "r");
	if (fp == NULL) {
		ERROR(ERR_SYSCALL, "popen", errno, strerror(errno));
		return -1;
	}

	do {
		status = fgets(mem_used, INT64_TO_STRING_MAX, fp);
	} while ((NULL == status) && (EINTR == errno));
	pclose(fp);
	if (NULL == status) {
		ERROR(ERR_SYSCALL, "fgets", errno, strerror(errno))
		return -1;
	}

	ret = strtol(mem_used, NULL, 10);
	if ((LONG_MAX != ret) && (LONG_MIN != ret) && (0 <= ret)) {
		return ret;
	} else {
		ERROR(ERR_LIBCALL, "strtol")
		return -1;
	}
}
