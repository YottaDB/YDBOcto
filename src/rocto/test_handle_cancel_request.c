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

#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <endian.h>
#include <sys/syscall.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_kill(pid_t pid, int signal) { return mock_type(int); }

int __wrap_get_pid_start_time(pid_t pid) { return mock_type(int); }

static void test_valid_cancel_request(void **state) {
	// Initialize dummy session
	RoctoSession session;
	ydb_buffer_t session_id;
	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	// Create buffers
	ydb_buffer_t  secret_key_list_buffer, secret_key_buffer, timestamp_buffer, pid_subs[2];
	ydb_buffer_t  z_status, z_status_value;
	ydb_buffer_t *pid_buffer = &pid_subs[0];
	char	      pid_str[INT32_TO_STRING_MAX], secret_key_str[INT32_TO_STRING_MAX];
	int	      secret_key = 0;
	YDB_LITERAL_TO_BUFFER("%ydboctoSecretKeyList", &secret_key_list_buffer);

	// Initialize secret key and pid buffers
	syscall(SYS_getrandom, &secret_key, 4, 0);
	snprintf(secret_key_str, INT32_TO_STRING_MAX, "%u", secret_key);
	YDB_STRING_TO_BUFFER(secret_key_str, &secret_key_buffer);
	YDB_LITERAL_TO_BUFFER("2000", pid_buffer);
	YDB_LITERAL_TO_BUFFER("timestamp", &pid_subs[1]);
	YDB_LITERAL_TO_BUFFER("1111", &timestamp_buffer);

	// Set local variable with secret key and pid
	int status = 0;
	status = ydb_set_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_buffer);
	YDB_ERROR_CHECK(status);
	status = ydb_set_s(&secret_key_list_buffer, 2, &pid_subs[0], &timestamp_buffer);
	YDB_ERROR_CHECK(status);

	// Create dummy CancelRequest
	CancelRequest cancel_request;
	cancel_request.length = sizeof(unsigned int) + sizeof(int) + sizeof(int) + sizeof(int);
	cancel_request.request_code = 80877102;
	cancel_request.pid = 2000;
	cancel_request.secret_key = secret_key;

	// Call the function
	int result = -1;
	will_return(__wrap_kill, 0);
	will_return(__wrap_get_pid_start_time, 1111);
	result = handle_cancel_request(&cancel_request);

	assert_int_equal(result, 0);
}

static void test_bad_pid(void **state) {
	// Initialize dummy session
	RoctoSession session;
	ydb_buffer_t session_id;
	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	// Create buffers
	ydb_buffer_t  secret_key_list_buffer, secret_key_buffer, timestamp_buffer, pid_subs[2];
	ydb_buffer_t *pid_buffer = &pid_subs[0];
	ydb_buffer_t  z_status, z_status_value;
	char	      pid_str[INT32_TO_STRING_MAX], secret_key_str[INT32_TO_STRING_MAX];
	int	      secret_key = 0;
	YDB_LITERAL_TO_BUFFER("%ydboctoSecretKeyList", &secret_key_list_buffer);

	// Initialize secret key and pid buffers
	syscall(SYS_getrandom, &secret_key, 4, 0);
	snprintf(secret_key_str, INT32_TO_STRING_MAX, "%u", secret_key);
	YDB_STRING_TO_BUFFER(secret_key_str, &secret_key_buffer);
	YDB_LITERAL_TO_BUFFER("2000", pid_buffer);
	YDB_LITERAL_TO_BUFFER("timestamp", &pid_subs[1]);
	YDB_LITERAL_TO_BUFFER("1111", &timestamp_buffer);

	// Set local variable with secret key and pid
	int status = 0;
	status = ydb_set_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_buffer);
	YDB_ERROR_CHECK(status);

	// Create dummy CancelRequest
	CancelRequest cancel_request;
	cancel_request.length = sizeof(unsigned int) + sizeof(int) + sizeof(int) + sizeof(int);
	cancel_request.request_code = 80877102;
	cancel_request.pid = 4000;
	cancel_request.secret_key = secret_key;

	// Call the function
	int result = 0;
	result = handle_cancel_request(&cancel_request);

	assert_int_equal(result, -1);
}

static void test_bad_timestamp(void **state) {
	// Initialize dummy session
	RoctoSession session;
	ydb_buffer_t session_id;
	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	// Create buffers
	ydb_buffer_t  secret_key_list_buffer, secret_key_buffer, timestamp_buffer, pid_subs[2];
	ydb_buffer_t *pid_buffer = &pid_subs[0];
	ydb_buffer_t  z_status, z_status_value;
	char	      pid_str[INT32_TO_STRING_MAX], secret_key_str[INT32_TO_STRING_MAX];
	int	      secret_key = 0;
	YDB_LITERAL_TO_BUFFER("%ydboctoSecretKeyList", &secret_key_list_buffer);

	// Initialize secret key and pid buffers
	syscall(SYS_getrandom, &secret_key, 4, 0);
	snprintf(secret_key_str, INT32_TO_STRING_MAX, "%u", secret_key);
	YDB_STRING_TO_BUFFER(secret_key_str, &secret_key_buffer);
	YDB_LITERAL_TO_BUFFER("2000", pid_buffer);
	YDB_LITERAL_TO_BUFFER("timestamp", &pid_subs[1]);
	YDB_LITERAL_TO_BUFFER("1111", &timestamp_buffer);

	// Set local variable with secret key and pid
	int status = 0;
	status = ydb_set_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_buffer);
	YDB_ERROR_CHECK(status);
	status = ydb_set_s(&secret_key_list_buffer, 2, &pid_subs[0], &timestamp_buffer);
	YDB_ERROR_CHECK(status);

	// Create dummy CancelRequest
	CancelRequest cancel_request;
	cancel_request.length = sizeof(unsigned int) + sizeof(int) + sizeof(int) + sizeof(int);
	cancel_request.request_code = 80877102;
	cancel_request.pid = 2000;
	cancel_request.secret_key = secret_key - 1;

	// Call the function
	int result = 0;
	will_return(__wrap_get_pid_start_time, 7777); // Bad timestamp
	result = handle_cancel_request(&cancel_request);

	assert_int_equal(result, -1);
}
static void test_bad_secret_key(void **state) {
	// Initialize dummy session
	RoctoSession session;
	ydb_buffer_t session_id;
	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	// Create buffers
	ydb_buffer_t  secret_key_list_buffer, secret_key_buffer, timestamp_buffer, pid_subs[2];
	ydb_buffer_t *pid_buffer = &pid_subs[0];
	ydb_buffer_t  z_status, z_status_value;
	char	      pid_str[INT32_TO_STRING_MAX], secret_key_str[INT32_TO_STRING_MAX];
	int	      secret_key = 0;
	YDB_LITERAL_TO_BUFFER("%ydboctoSecretKeyList", &secret_key_list_buffer);

	// Initialize secret key and pid buffers
	syscall(SYS_getrandom, &secret_key, 4, 0);
	snprintf(secret_key_str, INT32_TO_STRING_MAX, "%u", secret_key);
	YDB_STRING_TO_BUFFER(secret_key_str, &secret_key_buffer);
	YDB_LITERAL_TO_BUFFER("2000", pid_buffer);
	YDB_LITERAL_TO_BUFFER("timestamp", &pid_subs[1]);
	YDB_LITERAL_TO_BUFFER("1111", &timestamp_buffer);

	// Set local variable with secret key and pid
	int status = 0;
	status = ydb_set_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_buffer);
	YDB_ERROR_CHECK(status);
	status = ydb_set_s(&secret_key_list_buffer, 2, &pid_subs[0], &timestamp_buffer);
	YDB_ERROR_CHECK(status);

	// Create dummy CancelRequest
	CancelRequest cancel_request;
	cancel_request.length = sizeof(unsigned int) + sizeof(int) + sizeof(int) + sizeof(int);
	cancel_request.request_code = 80877102;
	cancel_request.pid = 2000;
	cancel_request.secret_key = secret_key - 1;

	// Call the function
	int result = 0;
	will_return(__wrap_get_pid_start_time, 1111);
	result = handle_cancel_request(&cancel_request);

	assert_int_equal(result, -1);
}

static void test_kill_failure(void **state) {
	// Initialize dummy session
	RoctoSession session;
	ydb_buffer_t session_id;
	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	// Create buffers
	ydb_buffer_t  secret_key_list_buffer, secret_key_buffer, timestamp_buffer, pid_subs[2];
	ydb_buffer_t *pid_buffer = &pid_subs[0];
	ydb_buffer_t  z_status, z_status_value;
	char	      pid_str[INT32_TO_STRING_MAX], secret_key_str[INT32_TO_STRING_MAX];
	int	      secret_key = 0;
	YDB_LITERAL_TO_BUFFER("%ydboctoSecretKeyList", &secret_key_list_buffer);

	// Initialize secret key and pid buffers
	syscall(SYS_getrandom, &secret_key, 4, 0);
	snprintf(secret_key_str, INT32_TO_STRING_MAX, "%u", secret_key);
	YDB_STRING_TO_BUFFER(secret_key_str, &secret_key_buffer);
	YDB_LITERAL_TO_BUFFER("2000", pid_buffer);
	YDB_LITERAL_TO_BUFFER("timestamp", &pid_subs[1]);
	YDB_LITERAL_TO_BUFFER("1111", &timestamp_buffer);

	// Set local variable with secret key and pid
	int status = 0;
	status = ydb_set_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_buffer);
	YDB_ERROR_CHECK(status);

	// Create dummy CancelRequest
	CancelRequest cancel_request;
	cancel_request.length = sizeof(unsigned int) + sizeof(int) + sizeof(int) + sizeof(int);
	cancel_request.request_code = 80877102;
	cancel_request.pid = 2000;
	cancel_request.secret_key = secret_key;

	// Call the function
	int result = 0;
	will_return(__wrap_get_pid_start_time, 1111);
	will_return(__wrap_kill, -1);
	result = handle_cancel_request(&cancel_request);

	assert_int_equal(result, -1);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_cancel_request), cmocka_unit_test(test_bad_pid),      cmocka_unit_test(test_bad_timestamp),
	    cmocka_unit_test(test_bad_secret_key),	 cmocka_unit_test(test_kill_failure),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
