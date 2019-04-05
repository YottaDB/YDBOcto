/* Copyright (C) 2018-2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length field
	char *message = "EXECUTE test(1, n)";
	message_length += strlen(message) + 1;		// count null
	message_length += sizeof(unsigned int);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	c += message_length - sizeof(unsigned int) - sizeof(unsigned int);
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
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length field
	char *message = "EXECUTE test(1, n)";
	message_length += strlen(message);		// exclude null
	message_length += sizeof(unsigned int);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	c += message_length - sizeof(unsigned int) - sizeof(unsigned int);
	*((int*)c) = htonl(10);	// set value of rows field

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

	free(test_data);
	free(execute);
	free_error_response(err);
}

static void test_unexpectedly_terminated_input(void **state) {
	// Test a single message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length field
	char *message = "EXECUTE test(1, n\0)";
	message_length += strlen(message) + 2;		// include early null
	message_length += sizeof(unsigned int);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	c += message_length - sizeof(unsigned int) - sizeof(unsigned int);
	*c = 10;	// set value of rows field

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

	free(test_data);
	free(execute);
	free_error_response(err);

}

static void test_missing_rows_field(void **state) {
	// Test a single message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length field
	char *message = "EXECUTE test(10, n)";
	message_length += strlen(message) + 1;		// count null
	char *c = NULL;
	ErrorResponse *err = NULL;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Execute;
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length field)
	strncpy(c, message, message_length - sizeof(unsigned int));

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

	free(test_data);
	free(execute);
}

static void test_invalid_type(void **state) {
	// Test a single message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length field
	char *message = "EXECUTE test(1, n)";
	message_length += strlen(message) + 1;		// count null
	message_length += sizeof(unsigned int);		// count rows field
	char *c = NULL;
	ErrorResponse *err = NULL;

	// Populate BaseMessage
	BaseMessage *test_data = malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = 'X';
	test_data->length = htonl(message_length);
	c = test_data->data;
	// Copy message (exclude length and rows fields)
	strncpy(c, message, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	c += message_length - sizeof(unsigned int) - sizeof(unsigned int);
	*((int*)c) = htonl(10);	// set value of rows field

	// The actual test
	Execute *execute = read_execute(test_data, &err);

	assert_non_null(err);
	assert_null(execute);

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
