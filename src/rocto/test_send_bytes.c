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

#include <openssl/ssl.h>

#include "rocto.h"
#include "message_formats.h"

int __wrap_send(int *socfd, void *buf, size_t len, int32_t flags) {
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

/* Note: the non-TLS tests below are not guarded by YDB_TLS_AVAILABLE. The non-TLS (i.e. plain "send()") code path in
 * send_bytes() is compiled unconditionally, so these tests are valid regardless of whether the TLS plugin is available.
 * Note also that YDB_TLS_AVAILABLE is always *defined* (to 1 or 0 by "src/CMakeLists.txt" via "src/constants.h.in"), so
 * any guard on it must use "#if YDB_TLS_AVAILABLE" and not "#ifdef"/"#ifndef", as the TLS-only block below does.
 */
static void test_valid_input_no_SSL(void **state) {
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

	assert_int_equal(rt, SOCK_OP_OK);
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

	will_return(__wrap_octo_log, ERROR_Severity);			      // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");				      // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);
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

	will_return(__wrap_octo_log, ERROR_Severity);			      // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");				      // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);
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

	will_return(__wrap_octo_log, ERROR_Severity); // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL);    // Expected error code
	will_return(__wrap_octo_log, "send");	      // Expected first va_arg

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_FAIL);
}

#if YDB_TLS_AVAILABLE
// send_bytes() delegates the TLS path entirely to the YDBTLS plugin (gtm_tls_send/gtm_tls_errno/gtm_tls_get_error),
// so those are the functions that need to be wrapped here rather than the underlying OpenSSL calls.
int __wrap_gtm_tls_send(gtm_tls_socket_t *tls_socket, char *buf, int send_len) {
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
	int32_t	     rt = 1;
	BaseMessage  message;
	uint32_t     length = sizeof(BaseMessage);
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_send, 10); // Successful result

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_OK);
}

static void test_connection_reset_with_SSL(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_send, -1); // send failed
	will_return(__wrap_gtm_tls_errno, ECONNRESET); // connection lost

	will_return(__wrap_octo_log, ERROR_Severity);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");					  // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);
}

static void test_broken_pipe_with_SSL(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_send, -1); // send failed
	will_return(__wrap_gtm_tls_errno, EPIPE); // pipe receiver lost

	will_return(__wrap_octo_log, ERROR_Severity);			  // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_UNEXPECTED_CLIENT_DISCONNECT); // Expected error code
	will_return(__wrap_octo_log, "");					  // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);
}

static void test_SSL_fatal_error(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_send, -1); // send failed
	will_return(__wrap_gtm_tls_errno, -1); // -1 signals a fatal, non-recoverable SSL_ERROR_SSL

	will_return(__wrap_gtm_tls_get_error, "SSL_ERROR_SSL");

	will_return(__wrap_octo_log, ERROR_Severity);		   // Expected error severity
	will_return(__wrap_octo_log, ERR_ROCTO_TLS_WRITE_FAILED); // Expected error code
	will_return(__wrap_octo_log, "SSL_ERROR_SSL");		   // Dummy error message

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_SHUTDOWN);
}

static void test_SSL_ERROR_SYSCALL(void **state) {
	int32_t	     rt = 1;
	BaseMessage  message;
	uint32_t     length = sizeof(BaseMessage);
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_send, -1); // send failed
	will_return(__wrap_gtm_tls_errno, EIO); // Arbitrary syscall error not otherwise handled

	will_return(__wrap_octo_log, ERROR_Severity);	    // Expected error severity
	will_return(__wrap_octo_log, ERR_SYSCALL);	    // Expected error code
	will_return(__wrap_octo_log, "ydb_tls_send()"); // First va_arg to ERROR()

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_FAIL);
}

static void test_send_socket_closed_with_SSL(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;
	session.ssl_active = TRUE;

	// Initialize relevant variables
	session.connection_fd = 0;

	will_return(__wrap_gtm_tls_send, 0); // No bytes sent - socket cleanly closed

	rt = send_bytes(&session, (char *)&message, length);

	assert_int_equal(rt, SOCK_OP_FAIL);
}
#endif

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input_no_SSL),	  cmocka_unit_test(test_send_connection_reset),
	    cmocka_unit_test(test_send_broken_pipe),	  cmocka_unit_test(test_send_timed_out),
#if YDB_TLS_AVAILABLE
	    cmocka_unit_test(test_valid_input_with_SSL),	   cmocka_unit_test(test_connection_reset_with_SSL),
	    cmocka_unit_test(test_broken_pipe_with_SSL),	   cmocka_unit_test(test_SSL_fatal_error),
	    cmocka_unit_test(test_SSL_ERROR_SYSCALL),		   cmocka_unit_test(test_send_socket_closed_with_SSL),
#endif
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
