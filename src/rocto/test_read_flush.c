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
	// Populate base message
	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage));
	test_data->type = PSQL_Flush;
	test_data->length = htonl(sizeof(uint32_t));

	// The actual test
	Flush *flush = read_flush(test_data);

	// Standard checks
	assert_non_null(flush);

	free(test_data);
	free(flush);
}

static void test_invalid_type(void **state) {
	// Populate base message
	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage));
	test_data->type = 'X';
	test_data->length = htonl(sizeof(uint32_t));

	// The actual test
	Flush *flush = read_flush(test_data);

	// Standard checks
	assert_null(flush);

	free(test_data);
}

static void test_invalid_length(void **state) {
	// Populate base message
	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage));
	test_data->type = PSQL_Flush;
	test_data->length = htonl(12);

	// The actual test
	Flush *flush = read_flush(test_data);

	// Standard checks
	assert_null(flush);

	free(test_data);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input),
	    cmocka_unit_test(test_invalid_type),
	    cmocka_unit_test(test_invalid_length),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
