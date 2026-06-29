/****************************************************************
 *								*
 * Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	*
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

int __wrap_recv(int *socfd, void *buf, size_t len, int32_t flags) {
	int32_t expected_return = mock_type(int);
	errno = mock_type(int);
	return expected_return;
}

void __wrap_octo_log(int line, char *file, enum VERBOSITY_LEVEL level, enum SEVERITY_LEVEL severity, enum ERROR error, ...) {
	char   *error_string = NULL;
	va_list args;
	va_start(args, error);
	int32_t expected_level = mock_type(int);
	int32_t expected_error = mock_type(int);
	char   *expected_error_string = mock_type(char *);

	assert_int_equal(severity, expected_level);
	assert_int_equal(error, expected_error);
	if (NULL != expected_error_string) {
		error_string = va_arg(args, char *);
		assert_string_equal(error_string, expected_error_string);
	}
	va_end(args);
}

static void test_valid_input_no_SSL(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 'X', buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, 10); // All bytes read
	will_return(__wrap_recv, 0);  // No error

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_OK);

	free(buffer);
}

static void test_invalid_read_size(void **state) {
	int32_t	     buffer_size = 10, bytes_to_read = -1;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	will_return(__wrap_octo_log, ERROR_Severity);	      // Expected error severity
	will_return(__wrap_octo_log, ERR_INVALID_READ_SIZE); // Expected error code
	will_return(__wrap_octo_log, NULL);		      // Not expecting a string, so indicate with NULL

	rt = read_bytes(&session, NULL, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, GENERIC_ERROR);
}

static void test_recv_interrupted(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	 // recv failed
	will_return(__wrap_recv, EINTR); // received interrupt

	will_return(__wrap_recv, 10); // continued to read all bytes
	will_return(__wrap_recv, 0);  // no error

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_OK);

	free(buffer);
}

static void test_recv_connection_reset(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	      // recv failed
	will_return(__wrap_recv, ECONNRESET); // connection lost

	will_return(__wrap_octo_log, ERROR_Severity);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");					  // Dummy error message

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);

	free(buffer);
}

static void test_recv_broken_pipe(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	 // recv failed
	will_return(__wrap_recv, EPIPE); // pipe receiver lost

	will_return(__wrap_octo_log, ERROR_Severity);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");					  // Dummy error message

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);

	free(buffer);
}

static void test_recv_timed_out(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	     // recv failed
	will_return(__wrap_recv, ETIMEDOUT); // connection timed out

	will_return(__wrap_octo_log, ERROR_Severity); // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL);    // Expected error code
	will_return(__wrap_octo_log, "read");	       // Expected first va_arg

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, -1);

	free(buffer);
}

static void test_socket_closed(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, 0); // No bytes read
	will_return(__wrap_recv, 0); // No error - socket cleanly closed

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);
	assert_int_equal(errno, 0);

	free(buffer);
}

#if YDB_TLS_AVAILABLE
// read_bytes() delegates the TLS path entirely to the YDBTLS plugin (gtm_tls_recv/gtm_tls_errno/gtm_tls_get_error),
// so those are the functions that need to be wrapped here rather than the underlying OpenSSL calls.
int __wrap_gtm_tls_recv(gtm_tls_socket_t *tls_socket, char *buf, int recv_len) {
	int32_t expected_return = mock_type(int);
	return expected_return;
}

int __wrap_gtm_tls_errno(void) {
	int32_t expected_return = mock_type(int);
	return expected_return;
}

const char *__wrap_gtm_tls_get_error(gtm_tls_socket_t *tls_socket) {
	char *expected_return = mock_type(char *);
	return expected_return;
}

static void test_valid_input_with_SSL(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 'X', buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_recv, 10); // Successful result

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_OK);

	free(buffer);
}

static void test_SSL_want_read_retry(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_recv, GTMTLS_WANT_READ); // Underlying TCP/IP pipe not yet ready

	will_return(__wrap_gtm_tls_recv, 10); // continued to read all bytes

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_OK);

	free(buffer);
}

static void test_SSL_read_interrupted(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_recv, -1);	 // gtm_tls_recv failed
	will_return(__wrap_gtm_tls_errno, EINTR); // received interrupt

	will_return(__wrap_gtm_tls_get_error, "EINTR"); // Queried unconditionally, but unused on this path

	will_return(__wrap_gtm_tls_recv, 10); // continued to read all bytes

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_OK);

	free(buffer);
}

static void test_SSL_connection_reset(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_recv, -1);		// gtm_tls_recv failed
	will_return(__wrap_gtm_tls_errno, ECONNRESET); // connection lost

	will_return(__wrap_gtm_tls_get_error, "ECONNRESET"); // Queried unconditionally, but unused on this path

	will_return(__wrap_octo_log, ERROR_Severity);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");					  // Dummy error message

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);

	free(buffer);
}

static void test_SSL_broken_pipe(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_recv, -1);	  // gtm_tls_recv failed
	will_return(__wrap_gtm_tls_errno, EPIPE); // pipe receiver lost

	will_return(__wrap_gtm_tls_get_error, "EPIPE"); // Queried unconditionally, but unused on this path

	will_return(__wrap_octo_log, ERROR_Severity);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");					  // Dummy error message

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);

	free(buffer);
}

static void test_SSL_read_failed(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 10, bytes_to_read = 10;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_recv, -1);	// gtm_tls_recv failed
	will_return(__wrap_gtm_tls_errno, EIO); // Arbitrary syscall error not otherwise handled

	will_return(__wrap_gtm_tls_get_error, "unknown (TLS)");

	will_return(__wrap_octo_log, ERROR_Severity);	     // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_TLS_READ_FAILED); // Expected error code
	will_return(__wrap_octo_log, "unknown (TLS)");	     // Dummy error message

	rt = read_bytes(&session, &buffer, &buffer_size, bytes_to_read, FALSE);

	assert_int_equal(rt, SOCK_OP_FAIL);

	free(buffer);
}
#endif

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input_no_SSL),	   cmocka_unit_test(test_invalid_read_size),
	    cmocka_unit_test(test_recv_interrupted),	   cmocka_unit_test(test_recv_connection_reset),
	    cmocka_unit_test(test_recv_broken_pipe),	   cmocka_unit_test(test_recv_timed_out),
	    cmocka_unit_test(test_socket_closed),
#if YDB_TLS_AVAILABLE
	    cmocka_unit_test(test_valid_input_with_SSL),  cmocka_unit_test(test_SSL_want_read_retry),
	    cmocka_unit_test(test_SSL_read_interrupted),  cmocka_unit_test(test_SSL_connection_reset),
	    cmocka_unit_test(test_SSL_broken_pipe),	   cmocka_unit_test(test_SSL_read_failed),
#endif
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
