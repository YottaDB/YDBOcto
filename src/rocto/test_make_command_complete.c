/* Copyright (C) 2017-2019 YottaDB, LLC
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
	CommandComplete *response = NULL;
	CommandComplete *received_response = NULL;
	ErrorResponse *err = NULL;

	char *message = "SELECT 5";
	int expected_length = sizeof(unsigned int) + strlen(message) + 1;

	response = make_command_complete(message);
	received_response = read_command_complete((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);
	assert_string_equal(received_response->command_tag, message);

	free(response);
	free(received_response);
}

static void test_null_input(void **state) {
	CommandComplete *response = NULL;
	CommandComplete *received_response = NULL;
	ErrorResponse *err = NULL;

	char *message = NULL;
	int expected_length = sizeof(unsigned int) + 1;		// count empty string null terminator

	response = make_command_complete(message);
	received_response = read_command_complete((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->length, expected_length);
	assert_string_equal(received_response->command_tag, "");

	free(response);
	free(received_response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_null_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
