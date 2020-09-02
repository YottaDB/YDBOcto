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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

int handle_cancel_request(CancelRequest *cancel_request) {
	TRACE(ERR_ENTERING_FUNCTION, "handle_cancel_request");
	ydb_buffer_t  secret_key_list_buffer, pid_subs[2];
	ydb_buffer_t *pid_buffer = &pid_subs[0];
	char	      pid_str[INT32_TO_STRING_MAX], secret_key_str[INT32_TO_STRING_MAX];
	unsigned int  ret_value, secret_key;
	int	      status;
	pid_t	      pid;
	pid = cancel_request->pid;
	secret_key = cancel_request->secret_key;

	// Populate buffers
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOSECRETKEYLIST, &secret_key_list_buffer);
	snprintf(pid_str, INT32_TO_STRING_MAX, "%u", cancel_request->pid); // pid
	YDB_STRING_TO_BUFFER(pid_str, pid_buffer);

	// Ensure data exists for PID
	status = ydb_data_s(&secret_key_list_buffer, 1, pid_buffer, &ret_value);
	YDB_ERROR_CHECK(status);
	if (11 != ret_value) {
		// Error NOT propagated to client for security reasons
		LOG_LOCAL_ONLY(ERROR, ERR_ROCTO_DB_LOOKUP, "handle_cancel_request", "pid");
		return -1;
	}

	// Confirm PID corresponds to original rocto process by checking against start timestamp
	// Initialize buffer for PID timestamp lookup result
	ydb_buffer_t timestamp_result;
	char	     timestamp_result_str[INT64_TO_STRING_MAX];
	timestamp_result.buf_addr = timestamp_result_str;
	timestamp_result.len_alloc = INT64_TO_STRING_MAX;
	YDB_LITERAL_TO_BUFFER(OCTOLIT_TIMESTAMP, &pid_subs[1]);

	// Retrieve timestamp for given PID
	status = ydb_get_s(&secret_key_list_buffer, 2, &pid_subs[0], &timestamp_result);
	YDB_ERROR_CHECK(status);
	// Add null terminator
	timestamp_result.buf_addr[timestamp_result.len_used] = '\0';

	// Validate current PID timestamp against stored PID timestamp
	char		   cur_timestamp_str[INT64_TO_STRING_MAX];
	unsigned long long cur_timestamp;
	cur_timestamp = get_pid_start_time(pid);
	if (0 == cur_timestamp) {
		// Error message emitted by callee get_pid_start_time()
		return -1;
	}
	snprintf(cur_timestamp_str, INT64_TO_STRING_MAX, "%llu", cur_timestamp);
	if (0 != strncmp(cur_timestamp_str, timestamp_result.buf_addr, INT64_TO_STRING_MAX)) {
		// Error NOT propagated to client for security reasons
		LOG_LOCAL_ONLY(ERROR, ERR_ROCTO_BAD_TIMESTAMP, "");
		return -1;
	}

	// Initialize buffer for secret key lookup result
	ydb_buffer_t secret_key_result;
	char	     secret_key_result_str[INT32_TO_STRING_MAX];
	secret_key_result.buf_addr = secret_key_result_str;
	secret_key_result.len_alloc = sizeof(secret_key_result_str);

	// Retrieve secret key for given PID
	status = ydb_get_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_result);
	YDB_ERROR_CHECK(status);
	// Add null terminator
	secret_key_result.buf_addr[secret_key_result.len_used] = '\0';

	// Validate received secret key against stored secret key
	snprintf(secret_key_str, INT32_TO_STRING_MAX, "%u", secret_key); // secret key
	if (0 != strncmp(secret_key_str, secret_key_result.buf_addr, INT32_TO_STRING_MAX)) {
		// Error NOT propagated to client for security reasons
		LOG_LOCAL_ONLY(ERROR, ERR_ROCTO_SECRET_KEY_MISMATCH, "");
		return -1;
	}

	// Send cancel signal (SIGUSR1) to target rocto session
	status = kill(pid, SIGUSR1);
	if (0 != status) {
		ERROR(ERR_SYSCALL, "kill()", errno, strerror(errno));
		return -1;
	}

	return 0;
}
