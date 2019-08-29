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
	// Test a single message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length field
	char *message = "EXECUTE test(1, n)";
	message_length += strlen(message) + 1;		// count null
	message_length += sizeof(uint32_t);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(uint32_t) - sizeof(uint32_t));
	c += message_length - sizeof(uint32_t) - sizeof(uint32_t);
	*((int*)c) = htonl(10);	// set value of rows field

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(execute);
	assert_null(err);

	free(test_data);
	free(execute);
}

static void test_non_terminated_input(void **state) {
	// Test a single message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length field
	char *message = "EXECUTE test(1, n)";
	message_length += strlen(message);		// exclude null
	message_length += sizeof(uint32_t);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(uint32_t) - sizeof(uint32_t));
	c += message_length - sizeof(uint32_t) - sizeof(uint32_t);
	*((int*)c) = htonl(-1);		// set value of rows field to enforce non-null-terminated string

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Execute", "source");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free(execute);
	free_error_response(err);
}

static void test_unexpectedly_terminated_input(void **state) {
	// Test a single message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length field
	char *message = "EXECUTE test(1, n\0)";
	message_length += strlen(message) + 2;		// include early null
	message_length += sizeof(uint32_t);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(uint32_t) - sizeof(uint32_t));
	c += message_length - sizeof(uint32_t) - sizeof(uint32_t);
	*c = 10;	// set value of rows field

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "Execute");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free(execute);
	free_error_response(err);

}

static void test_missing_rows_field(void **state) {
	// Test a single message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length field
	char *message = "EXECUTE test(10, n)";
	message_length += strlen(message) + 1;		// count null
	char *c = NULL;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length field)
	strncpy(c, message, message_length - sizeof(uint32_t));

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Execute", "number of rows to return");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}

static void test_invalid_type(void **state) {
	// Test a single message
	uint32_t message_length = 0;
	message_length += sizeof(uint32_t);		// count length field
	char *message = "EXECUTE test(1, n)";
	message_length += strlen(message) + 1;		// count null
	message_length += sizeof(uint32_t);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(uint32_t));
	test_data->type = 'X';
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(uint32_t) - sizeof(uint32_t));
	c += message_length - sizeof(uint32_t) - sizeof(uint32_t);
	*((int*)c) = htonl(10);	// set value of rows field

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Execute", test_data->type, PSQL_Execute);
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free(execute);
	free_error_response(err);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_non_terminated_input),
		   cmocka_unit_test(test_unexpectedly_terminated_input),
		   cmocka_unit_test(test_missing_rows_field),
		   cmocka_unit_test(test_invalid_type),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
