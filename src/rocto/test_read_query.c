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
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length member
	char *message = "SELECT * FROM names;\0";
	message_length += strlen(message) + 1;		// count null

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Query;
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(unsigned int));

	// The actual test
	ErrorResponse *err = NULL;
	Query *query = read_query(test_data, &err);

	// Standard checks
	assert_non_null(query);
	assert_null(err);
	assert_string_equal(message, query->query);

	free(query);
	free(test_data);
}

static void test_non_terminated_input(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length member
	char *message = "SELECT * FROM names;";
	message_length += strlen(message);		// exclude null for test case

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Query;
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(unsigned int));

	// The actual test
	ErrorResponse *err = NULL;
	Query *query = read_query(test_data, &err);

	// Standard checks
	assert_null(query);
	assert_non_null(err);

	free_error_response(err);
	free(test_data);
}

static void test_unexpectedly_terminated_input(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length member
	char *message = "SELECT * FROM names\0;";
	message_length += strlen(message) + 2;		// expecting extra char after null

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Query;
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(unsigned int));

	// The actual test
	ErrorResponse *err = NULL;
	Query *query = read_query(test_data, &err);

	// Standard checks
	assert_null(query);
	assert_non_null(err);

	free_error_response(err);
	free(test_data);
}

static void test_missing_query(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length member

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Query;
	test_data->length = htonl(message_length);

	// The actual test
	ErrorResponse *err = NULL;
	Query *query = read_query(test_data, &err);

	// Standard checks
	assert_null(query);
	assert_non_null(err);

	free_error_response(err);
	free(test_data);
}

static void test_invalid_type(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);		// count length member
	char *message = "SELECT * FROM names;";
	message_length += strlen(message) + 1;		// expecting extra char after null

	// Populate base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = 'X';
	test_data->length = htonl(message_length);
	strncpy(test_data->data, message, message_length - sizeof(unsigned int));

	// The actual test
	ErrorResponse *err = NULL;
	Query *query = read_query(test_data, &err);

	// Standard checks
	assert_null(query);
	assert_non_null(err);

	free_error_response(err);
	free(test_data);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_non_terminated_input),
		   cmocka_unit_test(test_unexpectedly_terminated_input),
		   cmocka_unit_test(test_missing_query),
		   cmocka_unit_test(test_invalid_type),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
