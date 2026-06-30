/****************************************************************
 *								*
 * Copyright (c) 2020-2026 YottaDB LLC and/or its subsidiaries.	*
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
#include <unistd.h>
#include <fcntl.h>

#include "octo.h"

/* The first whitespace-separated field of "/proc/self/statm" is the process's total program size
 * (virtual size) in pages. Multiplying by the page size (and dividing by 1024) yields the same value as
 * "ps -p <pid> -o vsize" in KiB, but without spawning a shell/ps/cut/tr pipeline to obtain it.
 */
#define PROC_SELF_STATM "/proc/self/statm"

// Returns the virtual memory size (in KiB) used by the current process
int64_t get_mem_usage(void) {
	int	statm_fd;
	int	save_errno;
	ssize_t bytes_read;
	long	page_size;
	long	vsize_pages;
	/* "/proc/self/statm" holds 7 space-separated numbers; this is far more than enough to hold them. */
	char statm_buf[128];

	statm_fd = open(PROC_SELF_STATM, O_RDONLY);
	if (-1 == statm_fd) {
		ERROR(ERR_SYSCALL_WITH_ARG, "open()", errno, strerror(errno), PROC_SELF_STATM);
		return -1;
	}

	/* Read with read() (not fgets()) so a YDB signal that interrupts the read can be handled and the read
	 * resumed cleanly. read() distinguishes data (>0), EOF (0), and an interrupted call (-1 with EINTR),
	 * unlike fgets() which returns NULL for both EOF and interruption and can lose already-read data.
	 * "/proc/self/statm" is a tiny pseudo-file, so a single successful read() returns all of it.
	 */
	do {
		bytes_read = read(statm_fd, statm_buf, sizeof(statm_buf) - 1);
		if (0 <= bytes_read)
			break; /* read the stats line (bytes_read == 0 would be an unexpected empty file) */
		if (EINTR != errno)
			break;	     /* a real read() error; reported after the file is closed below */
		ydb_eintr_handler(); /* interrupted by a deferred YDB signal; handle it and resume the read */
	} while (TRUE);
	save_errno = errno;

	close(statm_fd);

	if (0 >= bytes_read) {
		ERROR(ERR_SYSCALL, "read", save_errno, strerror(save_errno))
		return -1;
	}
	statm_buf[bytes_read] = '\0';

	page_size = sysconf(_SC_PAGESIZE);
	if (-1 == page_size) {
		ERROR(ERR_SYSCALL, "sysconf", errno, strerror(errno))
		return -1;
	}

	/* First field of "/proc/self/statm" is the virtual size in pages; convert to KiB to match "ps". */
	vsize_pages = strtol(statm_buf, NULL, 10);
	if ((LONG_MAX != vsize_pages) && (LONG_MIN != vsize_pages) && (0 <= vsize_pages)) {
		return (int64_t)vsize_pages * page_size / 1024;
	} else {
		ERROR(ERR_LIBCALL, "strtol")
		return -1;
	}
}
