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

#include <openssl/ssl.h>

#include "rocto.h"
#include "message_formats.h"

int __wrap_send(int *socfd, void *buf, size_t len, int32_t flags) {
	int32_t expected_return = mock_type(int);
	errno = mock_type(int);
	return expected_return;
}

int __wrap_SSL_write(SSL *ossl_connection, void *buf, size_t len) {
	int32_t expected_return = mock_type(int);
	errno = mock_type(int);
	return expected_return;
}

unsigned long __wrap_SSL_get_error(const SSL *ossl_connection, int32_t ret) {
	int32_t expected_return = mock_type(int);
	return expected_return;
}

unsigned long __wrap_ERR_peek_last_error() {
	int32_t expected_return = mock_type(int);
	return expected_return;
}

char *__wrap_ERR_error_string(unsigned long error_code, char *buf) {
	unsigned long expected_error_code = mock_type(unsigned long);
	char	     *expected_return = mock_type(char *);
	assert_int_equal(error_code, expected_error_code);
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
	int32_t	     rt = 1;
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	message.type = 'S';
	message.length = sizeof(uint32_t);

	will_return(__wrap_send, 10); // All bytes read
	will_return(__wrap_send, 0);  // No error

	rt = send_bytes(&session, (char *)&message, message.length + 1);

	assert_int_equal(rt, 0);
}

static void test_send_connection_reset(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = FALSE;

	session.connection_fd = 0;

	will_return(__wrap_send, -1);	      // send failed
	will_return(__wrap_send, ECONNRESET); // connection lost

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_send_broken_pipe(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_send, -1);	 // send failed
	will_return(__wrap_send, EPIPE); // pipe receiver lost

	will_return(__wrap_octo_log, FATAL);	   // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL); // Expected error code
	will_return(__wrap_octo_log, "send");	   // Expected first va_arg

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_send_timed_out(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_send, -1);	     // send failed
	will_return(__wrap_send, ETIMEDOUT); // connection timed out

	will_return(__wrap_octo_log, FATAL);	   // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL); // Expected error code
	will_return(__wrap_octo_log, "send");	   // Expected first va_arg

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_socket_closed(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = FALSE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_send, 0); // No bytes read
	will_return(__wrap_send, 0); // No error - socket cleanly closed

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
	assert_int_equal(errno, 0);
}

static void test_valid_input_with_SSL(void **state) {
	int32_t	     rt = 1;
	BaseMessage  message;
	uint32_t     length = sizeof(BaseMessage);
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_SSL_write, 10); // Successful result
	will_return(__wrap_SSL_write, 0);  // No error

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 0);
}

static void test_connection_reset_with_SSL(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	session.connection_fd = 0;

	will_return(__wrap_SSL_write, -1);	   // send failed
	will_return(__wrap_SSL_write, ECONNRESET); // connection lost

	will_return(__wrap_SSL_get_error, SSL_ERROR_ZERO_RETURN); // Arbitrary error code
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_ZERO_RETURN);

	will_return(__wrap_ERR_error_string, SSL_ERROR_ZERO_RETURN);
	will_return(__wrap_ERR_error_string, "SSL_ERROR_ZERO_RETURN");

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_SSL_write_interrupted(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_SSL_write, -1);    // SSL_write failed
	will_return(__wrap_SSL_write, EINTR); // received interrupt

	will_return(__wrap_SSL_get_error, SSL_ERROR_WANT_WRITE);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_WANT_WRITE);

	will_return(__wrap_ERR_error_string, SSL_ERROR_WANT_WRITE);
	will_return(__wrap_ERR_error_string, "SSL_ERROR_WANT_WRITE");

	will_return(__wrap_octo_log, FATAL);			   // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_OSSL_WRITE_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_WANT_WRITE");	   // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_SSL_ERROR_ZERO_RETURN(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_SSL_write, -1);  // SSL_write failed
	will_return(__wrap_SSL_write, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_ZERO_RETURN);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_ZERO_RETURN); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_ZERO_RETURN); // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "SSL_ERROR_ZERO_RETURN");

	will_return(__wrap_octo_log, FATAL);			   // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_OSSL_WRITE_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_ZERO_RETURN");	   // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_SSL_ERROR_WANT_WRITE(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_SSL_write, -1);  // SSL_write failed
	will_return(__wrap_SSL_write, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_WANT_READ);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_WANT_READ); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_WANT_READ); // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "SSL_ERROR_WANT_READ");

	will_return(__wrap_octo_log, FATAL);			   // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_OSSL_WRITE_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_WANT_READ");	   // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_SSL_ERROR_SYSCALL(void **state) {
	int32_t	     rt = 1;
	BaseMessage  message;
	uint32_t     length = sizeof(BaseMessage);
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_SSL_write, -1);  // SSL_write failed
	will_return(__wrap_SSL_write, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_SYSCALL);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_SYSCALL); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_SYSCALL);   // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "unknown (OpenSSL)"); // Arbitrary - not read in this test

	will_return(__wrap_octo_log, FATAL);		   // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL);	   // Expected error code
	will_return(__wrap_octo_log, "unknown (OpenSSL)"); // First va_arg to FATAL()

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

static void test_SSL_ERROR_SSL(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_SSL_write, -1);  // SSL_write failed
	will_return(__wrap_SSL_write, EIO); // Arbitrary error code

	will_return(__wrap_SSL_get_error, SSL_ERROR_SSL);
	will_return(__wrap_ERR_peek_last_error, SSL_ERROR_SSL); // Arbitrary - may not be reflected in integration

	will_return(__wrap_ERR_error_string, SSL_ERROR_SSL); // Arbitrary - may not be reflected in integration
	will_return(__wrap_ERR_error_string, "SSL_ERROR_SSL");

	will_return(__wrap_octo_log, FATAL);			   // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_OSSL_WRITE_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_SSL");		   // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, 1);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input_no_SSL),	  cmocka_unit_test(test_send_connection_reset),
	    cmocka_unit_test(test_send_broken_pipe),	  cmocka_unit_test(test_send_timed_out),
	    cmocka_unit_test(test_valid_input_with_SSL),  cmocka_unit_test(test_connection_reset_with_SSL),
	    cmocka_unit_test(test_SSL_write_interrupted), cmocka_unit_test(test_SSL_ERROR_ZERO_RETURN),
	    cmocka_unit_test(test_SSL_ERROR_WANT_WRITE),  cmocka_unit_test(test_SSL_ERROR_SYSCALL),
	    cmocka_unit_test(test_SSL_ERROR_SSL),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
