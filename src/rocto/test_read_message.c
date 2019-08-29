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

int __wrap_read_bytes(RoctoSession *session, char *message, int32_t buffer_size, int32_t bytes_to_read) {
	int32_t expected_return = mock_type(int);
	return expected_return;
}

static void test_valid_input(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	will_return(__wrap_read_bytes, 0);
	will_return(__wrap_read_bytes, 0);

	int32_t rocto_err = 0;
	result = read_message(NULL, (char*)buffer, 0, &rocto_err);

	assert_non_null(result);
	assert_int_equal(rocto_err, 0);

	free(buffer);
}

static void test_failed_first_read(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	// Test first read fails
	will_return(__wrap_read_bytes, -1);

	int32_t rocto_err = 0;
	result = read_message(NULL, (char*)buffer, 0, &rocto_err);

	assert_null(result);
	assert_int_equal(rocto_err, -1);

	free(buffer);
}

static void test_failed_second_read(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	// Test second read fails
	will_return(__wrap_read_bytes, 0);
	will_return(__wrap_read_bytes, -1);

	int32_t rocto_err = 0;
	result = read_message(NULL, (char*)buffer, 0, &rocto_err);

	assert_null(result);
	assert_int_equal(rocto_err, -1);

	free(buffer);
}

static void test_connection_reset_first_read(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	// Test second read fails
	will_return(__wrap_read_bytes, -2);

	int32_t rocto_err = 0;
	result = read_message(NULL, (char*)buffer, 0, &rocto_err);

	assert_null(result);
	assert_int_equal(rocto_err, -2);

	free(buffer);
}

static void test_connection_reset_second_read(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	// Test second read fails
	will_return(__wrap_read_bytes, 0);
	will_return(__wrap_read_bytes, -2);

	int32_t rocto_err = 0;
	result = read_message(NULL, (char*)buffer, 0, &rocto_err);

	assert_null(result);
	assert_int_equal(rocto_err, -2);

	free(buffer);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_failed_first_read),
		cmocka_unit_test(test_failed_second_read),
		cmocka_unit_test(test_connection_reset_second_read),
		cmocka_unit_test(test_connection_reset_first_read),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
