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
	// Test a single startup message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length member
	char *message = "bad_password";
	message_length += strlen(message) + 1;		// count null

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_PasswordMessage;
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(uint32_t));

	// The actual test
	ErrorResponse *err = NULL;
	PasswordMessage *password_message = read_password_message(test_data, &err);

	// Standard checks
	assert_non_null(password_message);
	assert_null(err);
	assert_string_equal(message, password_message->password);

	free(password_message);
	free(test_data);
}

static void test_non_terminated_input(void **state) {
	// Test a single startup message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length member
	char *message = "bad_password";
	message_length += strlen(message);		// exclude null for test case
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_PasswordMessage;
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(uint32_t));

	// The actual test
	ErrorResponse *err = NULL;
	PasswordMessage *password_message = read_password_message(test_data, &err);

	// Standard checks
	assert_null(password_message);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "PasswordMessage", "password");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_unexpectedly_terminated_input(void **state) {
	// Test a single startup message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length member
	char *message = "bad_passwor\0d";
	message_length += strlen(message) + 2;		// expecting extra char after null
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_PasswordMessage;
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(uint32_t));

	// The actual test
	ErrorResponse *err = NULL;
	PasswordMessage *password_message = read_password_message(test_data, &err);

	// Standard checks
	assert_null(password_message);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "PasswordMessage");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_missing_password(void **state) {
	// Test a single startup message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length member
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_PasswordMessage;
	test_data->length = htonl(message_length);

	// The actual test
	ErrorResponse *err = NULL;
	PasswordMessage *password_message = read_password_message(test_data, &err);

	// Standard checks
	assert_null(password_message);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "PasswordMessage", "password");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_invalid_type(void **state) {
	// Test a single startup message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length member
	char *message = "bad_password";
	message_length += strlen(message) + 1;		// expecting extra char after null
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = 'X';
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(uint32_t));

	// The actual test
	ErrorResponse *err = NULL;
	PasswordMessage *password_message = read_password_message(test_data, &err);

	// Standard checks
	assert_null(password_message);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "PasswordMessage", test_data->type, PSQL_PasswordMessage);
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_non_terminated_input),
		   cmocka_unit_test(test_unexpectedly_terminated_input),
		   cmocka_unit_test(test_missing_password),
		   cmocka_unit_test(test_invalid_type),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
