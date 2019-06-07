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
	CloseComplete *response = NULL;
	CloseComplete *received_response = NULL;
	ErrorResponse *err = NULL;

	int expected_length = sizeof(unsigned int);

	response = make_close_complete();
	received_response = read_close_complete((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_CloseComplete);
	assert_int_equal(received_response->length, expected_length);

	free(response);
	free(received_response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
