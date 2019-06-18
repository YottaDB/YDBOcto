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
	assert_string_equal(buffer, "password");
}

static void test_valid_input_one_field_populated(void **state) {
	char buffer[MAX_STR_CONST];
	char *row = "||||||||||password|";
	unsigned int buf_len = MAX_STR_CONST, row_len = 0, pw_len = 0;

	row_len = strnlen(row, MAX_STR_CONST);
	unsigned int value_len = get_user_column_value(buffer, buf_len, row, row_len, ROLPASSWORD);
	pw_len = strlen("password");
	assert_int_equal(value_len, pw_len);
	assert_string_equal(buffer, "password");
}

static void test_valid_input_buffer_too_small(void **state) {
	char buffer[5];
	char *row = "1|jon|super|inh|crer|cred|canl|repl|bypassrl|conn|password|valid";
	unsigned int buf_len = 5, row_len = 0, pw_len = 0;

	row_len = strnlen(row, MAX_STR_CONST);
	unsigned int value_len = get_user_column_value(buffer, buf_len, row, row_len, ROLPASSWORD);
	assert_int_equal(value_len, 0);
}

static void test_invalid_input_null_pointers(void **state) {
	char buffer[MAX_STR_CONST];
	char *row = "||||||||||password|";
	unsigned int buf_len = MAX_STR_CONST, row_len = 0, pw_len = 0;
	row_len = strnlen(row, MAX_STR_CONST);

	// Test for NULL buffer
	unsigned int value_len = get_user_column_value(NULL, buf_len, row, row_len, ROLPASSWORD);
	assert_int_equal(value_len, 0);

	// Test for NULL row
	value_len = get_user_column_value(buffer, buf_len, NULL, row_len, ROLPASSWORD);
	assert_int_equal(value_len, 0);
}

static void test_invalid_input_enum_too_large(void **state) {
	char buffer[MAX_STR_CONST];
	char *row = "||||||||||password|";
	unsigned int buf_len = MAX_STR_CONST, row_len = 0, pw_len = 0;
	row_len = strnlen(row, MAX_STR_CONST);

	unsigned int value_len = get_user_column_value(buffer, buf_len, row, row_len, 500);
	assert_int_equal(value_len, 0);
}

static void test_invalid_input_zero_lengths(void **state) {
	char buffer[MAX_STR_CONST];
	char *row = "||||||||||password|";
	unsigned int buf_len = MAX_STR_CONST, row_len = 0, pw_len = 0;
	row_len = strnlen(row, MAX_STR_CONST);

	unsigned int value_len = get_user_column_value(buffer, 0, row, row_len, ROLPASSWORD);
	assert_int_equal(value_len, 0);

	value_len = get_user_column_value(buffer, buf_len, row, 0, ROLPASSWORD);
	assert_int_equal(value_len, 0);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input_all_fields_populated),
		   cmocka_unit_test(test_valid_input_one_field_populated),
		   cmocka_unit_test(test_valid_input_buffer_too_small),
		   cmocka_unit_test(test_invalid_input_null_pointers),
		   cmocka_unit_test(test_invalid_input_enum_too_large),
		   cmocka_unit_test(test_invalid_input_zero_lengths),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
