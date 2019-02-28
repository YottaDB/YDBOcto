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
	message_length += sizeof(unsigned int);
	char *message = "SELECT * FROM names;";
	message_length += strlen(message);
	// Terminating null
	message_length += 1;
	char *c;
	ErrorResponse *err = NULL;

        BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - 5);
	test_data->type = PSQL_Query;
	test_data->length = htonl(message_length);
	strcpy(test_data->data, message);
	c = test_data->data;
	c += strlen(message);
	*c++ = '\0';
	

	// The actual test
	Query *query = read_query(test_data, &err);

	// Standard checks
	assert_non_null(query);
	assert_null(err);
	assert_string_equal(message, query->query);

	free(query);
}

static void test_non_terminated_input(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	char *message = "SELECT * FROM names;";
	message_length += strlen(message) - 3;
	char *c;
	ErrorResponse *err = NULL;

        BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - 5);
	test_data->type = PSQL_Query;
	test_data->length = htonl(message_length);
	strcpy(test_data->data, message);
	c = test_data->data;
	c += strlen(message);
	

	// The actual test
	Query *query = read_query(test_data, &err);

	// Standard checks
	assert_null(query);
	assert_non_null(err);

	free_error_response(err);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_non_terminated_input)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
