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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

static void test_valid_input(void **state) {
	// Test a single SSLRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5679
	uint32_t request_code = 80877103;
	uint32_t message_length = sizeof(SSLRequest);
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version)
	SSLRequest *test_data = (SSLRequest*)malloc(sizeof(SSLRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);

	// The actual test
	SSLRequest *ssl = read_ssl_request(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(ssl);
	assert_null(err);
	assert_int_equal(message_length, ssl->length);
	assert_int_equal(request_code, ssl->request_code);

	free(test_data);
	free(ssl);
}

static void test_invalid_length(void **state) {
	// Test a single SSLRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5679
	uint32_t request_code = 80877103;
	uint32_t message_length = 9;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	SSLRequest *test_data = (SSLRequest*)malloc(sizeof(SSLRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);

	// The actual test
	SSLRequest *ssl = read_ssl_request(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(ssl);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_INT_VALUE,
			"SSLRequest", "length", message_length, 8);
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}

static void test_invalid_request_code(void **state) {
	// Test a single SSLRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5679
	uint32_t request_code = 0x43219765;
	uint32_t message_length = sizeof(SSLRequest);
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	SSLRequest *test_data = (SSLRequest*)malloc(sizeof(SSLRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);

	// The actual test
	SSLRequest *ssl = read_ssl_request(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(ssl);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_INT_VALUE,
			"SSLRequest", "request code", request_code, 80877103);
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}
int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
			cmocka_unit_test(test_valid_input),
			cmocka_unit_test(test_invalid_length),
			cmocka_unit_test(test_invalid_request_code),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
