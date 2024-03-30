/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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
#include "ydb_tls_interface.h"

int __wrap_recv(int *socfd, void *buf, size_t len, int32_t flags) {
	int32_t expected_return = mock_type(int);
	errno = mock_type(int);
	return expected_return;
}

int __wrap_gtm_tls_recv(gtm_tls_socket_t *tls_socket, void *buf, size_t len) {
	int32_t expected_return = mock_type(int);
	errno = mock_type(int);
	return expected_return;
}

#ifndef GTM_TLS_API_VERSION_GET_ERROR
unsigned long __wrap_gtm_tls_get_error() {
	int32_t expected_return = mock_type(int);
	return expected_return;
}
#else
unsigned long __wrap_gtm_tls_get_error(gtm_tls_socket_t *tls_socket) {
	int32_t expected_return = mock_type(int);
	return expected_return;
}
#endif

unsigned long __wrap_gtm_tls_errno() {
	int32_t expected_return = mock_type(int);
	return expected_return;
}

void __wrap_octo_log(int line, char *file, enum VERBOSITY_LEVEL level, enum SEVERITY_LEVEL severity, enum ERROR error, ...) {
	char   *error_string = NULL;
	va_list args;
	va_start(args, error);
	int32_t expected_level = mock_type(int);
	int32_t expected_error = mock_type(int);
	char   *expected_error_string = mock_type(char *);

	assert_int_equal(level, expected_level);
	assert_int_equal(error, expected_error);
	if (NULL != expected_error_string) {
		error_string = va_arg(args, char *);
		assert_string_equal(error_string, expected_error_string);
	}
	va_end(args);
}

static void test_valid_input_no_SSL(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Bytes match buffer size
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 'X', buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, 10); // All bytes read
	will_return(__wrap_recv, 0);  // No error

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 0);

	free(buffer);
}

static void test_read_too_large(void **state) {
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Bytes larger than buffer size
	buffer_size = 10;
	bytes_to_read = 100;

	will_return(__wrap_octo_log, WARNING);		  // Expected error severity
	will_return(__wrap_octo_log, ERR_READ_TOO_LARGE); // Expected error code
	will_return(__wrap_octo_log, NULL);		  // Not expecting a string, so indicate with NULL

	rt = read_bytes(&session, NULL, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);
}

static void test_invalid_read_size(void **state) {
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Bytes less than 0
	buffer_size = 10;
	bytes_to_read = -1;

	will_return(__wrap_octo_log, WARNING);		     // Expected error severity
	will_return(__wrap_octo_log, ERR_INVALID_READ_SIZE); // Expected error code
	will_return(__wrap_octo_log, NULL);		     // Not expecting a string, so indicate with NULL

	rt = read_bytes(&session, NULL, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);
}

static void test_recv_interrupted(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	 // recv failed
	will_return(__wrap_recv, EINTR); // received interrupt

	will_return(__wrap_recv, 10); // continued to read all bytes
	will_return(__wrap_recv, 0);  // no error

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 0);

	free(buffer);
}

static void test_recv_connection_reset(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	      // recv failed
	will_return(__wrap_recv, ECONNRESET); // connection lost

	will_return(__wrap_octo_log, WARNING);	   // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL); // Expected error code
	will_return(__wrap_octo_log, "read");	   // Expected first va_arg

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -2);

	free(buffer);
}

static void test_recv_broken_pipe(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	 // recv failed
	will_return(__wrap_recv, EPIPE); // pipe receiver lost

	will_return(__wrap_octo_log, WARNING);	   // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL); // Expected error code
	will_return(__wrap_octo_log, "read");	   // Expected first va_arg

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);

	free(buffer);
}

static void test_recv_timed_out(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, -1);	     // recv failed
	will_return(__wrap_recv, ETIMEDOUT); // connection timed out

	will_return(__wrap_octo_log, WARNING);	   // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL); // Expected error code
	will_return(__wrap_octo_log, "read");	   // Expected first va_arg

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);

	free(buffer);
}
static void test_socket_closed(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = FALSE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, 0); // No bytes read
	will_return(__wrap_recv, 0); // No error - socket cleanly closed

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -2);
	assert_int_equal(errno, 0);

	free(buffer);
}

static void test_valid_input_with_SSL(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Bytes match buffer size
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 'X', buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_SSL_read, 10); // Successful result
	will_return(__wrap_SSL_read, 0);  // No error

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 0);

	free(buffer);
}

static void test_SSL_read_interrupted(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_SSL_read, -1);    // SSL_read failed
	will_return(__wrap_SSL_read, EINTR); // received interrupt

	will_return(__wrap_SSL_get_error, SSL_ERROR_WANT_READ);

	will_return(__wrap_SSL_read, 10); // continued to read all bytes
	will_return(__wrap_SSL_read, 0);  // no error

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 0);

	free(buffer);
}

static void test_SSL_ERROR_ZERO_RETURN(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_SSL_read, -1);  // SSL_read failed
	will_return(__wrap_SSL_read, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_ZERO_RETURN);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_ZERO_RETURN); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_ZERO_RETURN); // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "SSL_ERROR_ZERO_RETURN");

	will_return(__wrap_octo_log, WARNING);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_OSSL_READ_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_ZERO_RETURN");	  // Dummy error message

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);

	free(buffer);
}

static void test_SSL_ERROR_WANT_READ(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_SSL_read, -1);  // SSL_read failed
	will_return(__wrap_SSL_read, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_WANT_READ);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_WANT_READ); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_WANT_READ); // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "SSL_ERROR_WANT_READ");

	will_return(__wrap_octo_log, WARNING);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_OSSL_READ_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_WANT_READ");	  // Dummy error message

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);

	free(buffer);
}

static void test_SSL_ERROR_SYSCALL(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_SSL_read, -1);  // SSL_read failed
	will_return(__wrap_SSL_read, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_SYSCALL);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_SYSCALL); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_SYSCALL);   // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "unknown (OpenSSL)"); // Arbitrary - not read in this test

	will_return(__wrap_octo_log, FATAL);		   // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL);	   // Expected error code
	will_return(__wrap_octo_log, "unknown (OpenSSL)"); // First va_arg to FATAL()

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);

	free(buffer);
}

static void test_SSL_ERROR_SSL(void **state) {
	char	    *buffer;
	int32_t	     buffer_size = 0, bytes_to_read = 0;
	int32_t	     rt = 1;
	RoctoSession session;
	session.ssl_active = TRUE;

	// valid input
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_SSL_read, -1);  // SSL_read failed
	will_return(__wrap_SSL_read, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_SSL);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_SSL); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_SSL); // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "SSL_ERROR_SSL");

	will_return(__wrap_octo_log, FATAL);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_OSSL_READ_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_SSL");		  // Dummy error message

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, -1);

	free(buffer);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input_no_SSL),	  cmocka_unit_test(test_read_too_large),
	    cmocka_unit_test(test_invalid_read_size),	  cmocka_unit_test(test_recv_interrupted),
	    cmocka_unit_test(test_recv_connection_reset), cmocka_unit_test(test_recv_broken_pipe),
	    cmocka_unit_test(test_recv_timed_out),	  cmocka_unit_test(test_socket_closed),
	    cmocka_unit_test(test_valid_input_with_SSL),  cmocka_unit_test(test_SSL_read_interrupted),
	    cmocka_unit_test(test_SSL_ERROR_ZERO_RETURN), cmocka_unit_test(test_SSL_ERROR_WANT_READ),
	    cmocka_unit_test(test_SSL_ERROR_SYSCALL),	  cmocka_unit_test(test_SSL_ERROR_SSL),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
