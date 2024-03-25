/****************************************************************
 *								*
 * Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* The command string below is approximately 50 characters in length, so combine with INT64_TO_STRING_MAX to get total length of the
 * command string.
 */
#define COMMAND_STRING "ps -p %d -o vsize | cut -d ' ' -f 2 | tr -d '\\n'"
#define COMMAND_LEN    (sizeof(COMMAND_STRING) + INT64_TO_STRING_MAX)

// Returns the amount of memory used by the current process
int64_t get_mem_usage(void) {
	FILE   *fp;
	char	mem_used[INT64_TO_STRING_MAX];
	int64_t ret;
	int	save_errno;
	char   *status;
	char	command[COMMAND_LEN];

	snprintf(command, sizeof(command), COMMAND_STRING, getpid());

	fp = popen(command, "r");
	if (fp == NULL) {
		ERROR(ERR_SYSCALL_WITH_ARG, "popen()", errno, strerror(errno), command);
		return -1;
	}

	do {
		status = fgets(mem_used, INT64_TO_STRING_MAX, fp);
		if (NULL != status)
			break;
		if (EINTR != errno)
			break;
		ydb_eintr_handler(); /* Needed to invoke YDB signal handler (for signal that caused
				      * EINTR) in a deferred but timely fashion.
				      */
	} while (TRUE);
	save_errno = errno;
	pclose(fp);
	if (NULL == status) {
		ERROR(ERR_SYSCALL, "fgets", save_errno, strerror(save_errno))
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
