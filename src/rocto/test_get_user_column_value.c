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

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

static void test_valid_input_all_fields_populated(void **state) {
	char buffer[MAX_STR_CONST];
	char *row = "1|jon|super|inh|crer|cred|canl|repl|bypassrl|conn|password|valid";
	unsigned int buf_len = MAX_STR_CONST, row_len = 0, pw_len = 0;

	row_len = strnlen(row, MAX_STR_CONST);
	unsigned int value_len = get_user_column_value(buffer, buf_len, row, row_len, ROLPASSWORD);
	pw_len = strlen("password");
	assert_int_equal(value_len, pw_len);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input_all_fields_populated),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
