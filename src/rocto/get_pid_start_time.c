/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"
#include "errors.h"
#include "octo_types.h"

// Expects buffer to contain output of /proc/[pid]/stat
int get_field_start(char *buffer, int num_spaces) {
	int i = 0, space_count = 0;

	for (i = 0; buffer[i]; i++) {
		if (buffer[i] == ' ') {
			space_count++;
			if (space_count >= num_spaces) {
				return i + 1; // Increment from last space before target field to the first char of that field
			}
		}
	}
	return -1;
}

/* Retrieves the start time of process with specified PID relative to time of system boot.
 * Returns non-zero time in most cases (normal return).
 * Returns 0 time in rare cases if child process has already terminated (normal return).
 * Returns 0 time in case of an error (abnormal return).
 */
unsigned long long get_pid_start_time(pid_t pid) {
	int		   field_start, stat_fd, result;
	int		   stat_buf_size = 8192;
	char		   file_path[80], stat_buf[stat_buf_size]; // Default buffer sizes should handle normal cases
	unsigned long long start_time;

	if (0 >= pid) {
		ERROR(ERR_ROCTO_INVALID_INT_VALUE, "get_pid_start_time", "pid", pid, "greater than 0");
		return 0;
	}
	// Create buffer with target file path (length of pid string + strlen("/proc//stat") + null)
	snprintf(file_path, INT32_TO_STRING_MAX + 12, "/proc/%u/stat", pid);
	// Read the file
	stat_fd = open(file_path, O_RDONLY);
	if (0 > stat_fd) {
		/* Check if the "open()" failed because the file is missing (i.e. errno is "ENOENT"). If so,
		 * the only way we know this as possible is if the child (rocto server) pid, that was created
		 * just now in the caller of this function using a "fork()" call, already terminated (very soon
		 * after it was created) and the "rocto_helper_waitpid()" thread in the parent already did a "wait()"
		 * call so the child's exit status was reaped. In this case, there is no need to register the pid
		 * start time as there is no need to handle any cancel query requests for this child pid. Therefore
		 * return without an error in this case. If not "ENOENT" though, record an error in the log file.
		 */
		if (ENOENT != errno) {
			ERROR(ERR_SYSCALL, "open()", errno, strerror(errno));
		}
		return 0;
	}
	result = read(stat_fd, stat_buf, stat_buf_size);
	// Handle errors
	if (result < 0) {
		ERROR(ERR_SYSCALL, "read()", errno, strerror(errno));
		close(stat_fd);
		return 0;
	}
	if (result >= stat_buf_size) {
		ERROR(ERR_INVALID_READ_SIZE, result);
		close(stat_fd);
		return 0;
	}

	// We want the 22nd field, i.e. the field after the 21st space
	field_start = get_field_start(stat_buf, 21);
	if (0 > field_start) {
		ERROR(ERR_ROCTO_TOO_MANY_VALUES, "get_pid_start_time", "spaces");
		close(stat_fd);
		return 0;
	}
	// Terminate the string at the next space
	stat_buf[get_field_start(stat_buf, 22) - 1] = '\0';
	// Extract the time value
	sscanf(stat_buf + field_start, "%llu", &start_time);
	close(stat_fd);

	return start_time;
}
