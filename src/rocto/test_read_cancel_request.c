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

int __wrap_read_bytes(RoctoSession *session, char **buffer, int *buffer_size, int bytes_to_read) {
	int pid = mock_type(int);
	int secret_key = mock_type(int);
	memcpy(*buffer, &pid, bytes_to_read);
	memcpy(&((*buffer)[sizeof(int)]), &secret_key, bytes_to_read);
	return 0;
}

static void test_valid_input(void **state) {
	// Test a single CancelRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5679
	unsigned int request_code = 80877102;
	unsigned int message_length = sizeof(CancelRequest);

	// Length + extra stuff - already counted (length, protocol version)
	CancelRequest *test_data = (CancelRequest *)malloc(sizeof(CancelRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);
	// The pid and secret_key values are arbitrary as they're not checked in read_cancel_request
	test_data->pid = htonl(7777);
	test_data->secret_key = htonl(8888);

	// The actual test
	will_return(__wrap_read_bytes, test_data->pid);
	will_return(__wrap_read_bytes, test_data->secret_key);
	CancelRequest *cancel = read_cancel_request(NULL, (char *)(&test_data->length), sizeof(test_data));

	// Standard checks
	assert_non_null(cancel);
	assert_int_equal(message_length, cancel->length);
	assert_int_equal(request_code, cancel->request_code);
	assert_int_equal(ntohl(test_data->pid), cancel->pid);
	assert_int_equal(ntohl(test_data->secret_key), cancel->secret_key);

	free(test_data);
	free(cancel);
}

static void test_invalid_length(void **state) {
	// Test a single CancelRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5678
	unsigned int request_code = 80877102;
	unsigned int message_length = 17;

	// Length + extra stuff - already counted (length, protocol version)
	CancelRequest *test_data = (CancelRequest *)malloc(sizeof(CancelRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);
	// The pid and secret_key values are arbitrary as they're not checked in read_cancel_request
	test_data->pid = htonl(7777);
	test_data->secret_key = htonl(8888);

	// The actual test
	CancelRequest *cancel = read_cancel_request(NULL, (char *)(&test_data->length), sizeof(test_data));

	// Standard checks
	assert_null(cancel);

	free(test_data);
}

static void test_invalid_request_code(void **state) {
	// Test a single CancelRequest message
	// most significant 16 bits: 1234, least significant 16 bits: 5678
	unsigned int request_code = 0x43219765;
	unsigned int message_length = sizeof(CancelRequest);

	// Length + extra stuff - already counted (length, protocol version)
	CancelRequest *test_data = (CancelRequest *)malloc(sizeof(CancelRequest));
	test_data->length = htonl(message_length);
	test_data->request_code = htonl(request_code);
	// The pid and secret_key values are arbitrary as they're not checked in read_cancel_request
	test_data->pid = htonl(7777);
	test_data->secret_key = htonl(8888);

	// The actual test
	CancelRequest *cancel = read_cancel_request(NULL, (char *)(&test_data->length), sizeof(test_data));

	// Standard checks
	assert_null(cancel);

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
