/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

static void test_select_statement(void **state) {
	CommandComplete *response = NULL;
	CommandComplete *received_response = NULL;

	char *	message = "SELECT 5";
	int32_t expected_length = sizeof(uint32_t) + strlen(message) + 1;

	response = make_command_complete(select_STATEMENT, 5);
	received_response = read_command_complete((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);
	assert_string_equal(received_response->command_tag, message);

	free(response);
	free(received_response);
}

static void test_set_statement(void **state) {
	CommandComplete *response = NULL;
	CommandComplete *received_response = NULL;

	char *	message = "SET";
	int32_t expected_length = sizeof(uint32_t) + strlen(message) + 1;

	response = make_command_complete(set_STATEMENT, 0);
	received_response = read_command_complete((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);
	assert_string_equal(received_response->command_tag, message);

	free(response);
	free(received_response);
}

static void test_show_statement(void **state) {
	CommandComplete *response = NULL;
	CommandComplete *received_response = NULL;

	char *	message = "SHOW";
	int32_t expected_length = sizeof(uint32_t) + strlen(message) + 1;

	response = make_command_complete(show_STATEMENT, 0);
	received_response = read_command_complete((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);
	assert_string_equal(received_response->command_tag, message);

	free(response);
	free(received_response);
}

static void test_no_command_tag_statement(void **state) {
	CommandComplete *response = NULL;

	response = make_command_complete(discard_all_STATEMENT, 0);
	assert_null(response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_select_statement),
	    cmocka_unit_test(test_set_statement),
	    cmocka_unit_test(test_show_statement),
	    cmocka_unit_test(test_no_command_tag_statement),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
