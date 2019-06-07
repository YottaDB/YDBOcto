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
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <errno.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

int __wrap_recv(int *socfd, void *buf, size_t len, int flags) {
	int expected_return = mock_type(int);
	errno = mock_type(int);
	return expected_return;
}

static void test_valid_input(void **state) {
	char *buffer;
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;
	RoctoSession session;

	// Bytes match buffer size
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 'X', buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, 10);	// All bytes read
	will_return(__wrap_recv, 0);	// No error

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 0);

	free(buffer);
}

static void test_read_too_large(void **state) {
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;

	// Bytes larger than buffer size
	buffer_size = 10;
	bytes_to_read = 100;

	rt = read_bytes(NULL, NULL, buffer_size, bytes_to_read);

	assert_int_equal(rt, 1);
}

static void test_invalid_read_size(void **state) {
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;

	// Bytes less than 0
	buffer_size = 10;
	bytes_to_read = -1;

	rt = read_bytes(NULL, NULL, buffer_size, bytes_to_read);

	assert_int_equal(rt, 1);
}

static void test_recv_interrupted(void **state) {
	char *buffer;
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;
	RoctoSession session;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);		// recv failed
	will_return(__wrap_recv, EINTR);	// received interrupt

	will_return(__wrap_recv, 10);	// continued to read all bytes
	will_return(__wrap_recv, 0);	// no error

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 0);

	free(buffer);
}

static void test_recv_connection_reset(void **state) {
	char *buffer;
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;
	RoctoSession session;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);		// recv failed
	will_return(__wrap_recv, ECONNRESET);	// connection lost

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 1);

	free(buffer);
}

static void test_recv_broken_pipe(void **state) {
	char *buffer;
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;
	RoctoSession session;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);		// recv failed
	will_return(__wrap_recv, EPIPE);	// pipe receiver lost

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 1);

	free(buffer);
}

static void test_recv_timed_out(void **state) {
	char *buffer;
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;
	RoctoSession session;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);		// recv failed
	will_return(__wrap_recv, ETIMEDOUT);	// connection timed out

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 1);

	free(buffer);
}
static void test_socket_closed(void **state) {
	char *buffer;
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;
	RoctoSession session;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, 0);	// No bytes read
	will_return(__wrap_recv, 0);	// No error - socket cleanly closed

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 1);
	assert_int_equal(errno, 0);

	free(buffer);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_read_too_large),
		cmocka_unit_test(test_invalid_read_size),
		cmocka_unit_test(test_recv_interrupted),
		cmocka_unit_test(test_recv_connection_reset),
		cmocka_unit_test(test_recv_broken_pipe),
		cmocka_unit_test(test_recv_timed_out),
		cmocka_unit_test(test_socket_closed),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
