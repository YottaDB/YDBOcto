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
	CommandComplete *response = NULL;
	CommandComplete *received_response = NULL;

	char *message = "SELECT 5";
	int32_t expected_length = sizeof(uint32_t) + strlen(message) + 1;

	response = make_command_complete(message);
	received_response = read_command_complete((BaseMessage*)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);
	assert_string_equal(received_response->command_tag, message);

	free(response);
	free(received_response);
}

static void test_null_input(void **state) {
	CommandComplete *response = NULL;
	CommandComplete *received_response = NULL;

	char *message = NULL;
	int32_t expected_length = sizeof(uint32_t) + 1;		// count empty string null terminator

	response = make_command_complete(message);
	received_response = read_command_complete((BaseMessage*)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);
	assert_string_equal(received_response->command_tag, "");

	free(response);
	free(received_response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_null_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
