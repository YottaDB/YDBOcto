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
	ParameterStatus *response = NULL;
	ParameterStatus *received_response = NULL;
	ErrorResponse *err = NULL;
	StartupMessageParm parm = {"user","Alice"};

	// Expected length is length field + parameter strings, including null terminators
	int expected_length = sizeof(unsigned int) + strlen(parm.name) + sizeof(char) + strlen(parm.value) + sizeof(char);

	response = make_parameter_status(&parm);
	received_response = read_parameter_status((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_null(err);
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);

	char *c = received_response->data;
	assert_string_equal(c, parm.name);
	c += strlen(parm.name) + sizeof(char);
	assert_string_equal(c, parm.value);

	free(response);
	free(received_response);
}

static void test_null_input(void **state) {
	ParameterStatus *response = NULL;
	ParameterStatus *received_response = NULL;
	StartupMessageParm *parm = NULL;
	ErrorResponse *err = NULL;

	// Expected length is length field, since no parameter is passed
	int expected_length = sizeof(unsigned int);

	response = make_parameter_status(parm);
	received_response = read_parameter_status((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_null(err);
	assert_null(response);
	assert_null(received_response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_null_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
