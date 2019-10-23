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
	AuthenticationOk *response = NULL;
	AuthenticationOk *received_response = NULL;
	int32_t expected_type = 'R';
	uint32_t expected_length = sizeof(uint32_t) + sizeof(int);
	int32_t expected_result = 0;

	response = make_authentication_ok();
	received_response = read_authentication_ok((BaseMessage*)response);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, expected_type);
	assert_int_equal(received_response->length, htonl(expected_length));
	assert_int_equal(received_response->result, htonl(expected_result));

	free(response);
	free(received_response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
