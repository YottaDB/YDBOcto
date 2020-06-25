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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

static void test_valid_input(void **state) {
	// Test a single SSLRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5679
	uint32_t request_code = 80877103;
	uint32_t message_length = sizeof(SSLRequest);

	// Length + extra stuff - already counted (length, protocol version)
	SSLRequest *test_data = (SSLRequest *)malloc(sizeof(SSLRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);

	// The actual test
	SSLRequest *ssl = read_ssl_request(NULL, (char *)(&test_data->length), message_length);

	// Standard checks
	assert_non_null(ssl);
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

	// Length + extra stuff - already counted (length, protocol version)
	SSLRequest *test_data = (SSLRequest *)malloc(sizeof(SSLRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);

	// The actual test
	SSLRequest *ssl = read_ssl_request(NULL, (char *)(&test_data->length), message_length);

	// Standard checks
	assert_null(ssl);

	free(test_data);
}

static void test_invalid_request_code(void **state) {
	// Test a single SSLRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5679
	uint32_t request_code = 0x43219765;
	uint32_t message_length = sizeof(SSLRequest);

	// Length + extra stuff - already counted (length, protocol version)
	SSLRequest *test_data = (SSLRequest *)malloc(sizeof(SSLRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);

	// The actual test
	SSLRequest *ssl = read_ssl_request(NULL, (char *)(&test_data->length), message_length);

	// Standard checks
	assert_null(ssl);

	free(test_data);
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
