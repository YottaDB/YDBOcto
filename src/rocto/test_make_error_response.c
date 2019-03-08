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
	// The actual test
	char *message = "Seems OK to me man", *detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};
	ErrorResponse *response = make_error_response(PSQL_Error_ERROR,
						      PSQL_Code_Success,
						      message,
						      1, &a);
	// Expected length is each string + null terminating bytes + format +
	//  length part (5) + type (1)
	int expected_length = strlen("ERROR")
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success])
		+ strlen(message) + strlen(detail) + 4 + 4 + 4 + 1;
	// Standard checks
	assert_non_null(response);
	assert_int_equal(response->length, htonl(expected_length));

	free(response->args);
	free(response);
}

static void test_with_no_additional_parms(void **state) {
	// The actual test
	char *message = "Seems OK to me man", *detail = "This is a more complicated message";
	ErrorResponse *response = make_error_response(PSQL_Error_ERROR,
						      PSQL_Code_Success,
						      message, 0);
	// Expected length is each string + null terminating bytes(3) + format(3) +
	//  length part (4) + final null byte (1)
	int expected_length = strlen("ERROR")
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success])
		+ strlen(message) + 4 + 3 + 3 + 1;
	// Standard checks
	assert_non_null(response);
	assert_int_equal(response->length, htonl(expected_length));

	free(response->args);
	free(response);
}

static void test_with_additional_parms(void **state) {
	// The actual test
	char *message = "Seems OK to me man", *detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};
	ErrorResponse *response = make_error_response(PSQL_Error_ERROR,
						      PSQL_Code_Success,
						      message, 10, &a, &a, &a,
						      &a, &a, &a, &a, &a, &a, &a);
	// Total of 13 items; 10 additional, required 3
	int expected_length = strlen("ERROR")
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success])
		+ strlen(message) + strlen(detail)*10 + 4 + 13 + 13 + 1;
	// Standard checks
	assert_non_null(response);
	assert_int_equal(response->length, htonl(expected_length));

	free(response->args);
	free(response);
}

static void test_verify_args_pointers_correct(void **state) {
	char *message = "Seems OK to me man", *detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};
	ErrorResponse *response = make_error_response(PSQL_Error_ERROR,
						      PSQL_Code_Success,
						      message,
						      1, &a);
	// Expected length is each string + null terminating bytes + format +
	//  length part (5) + type (1)
	int expected_length = strlen("ERROR")
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success])
		+ strlen(message) + strlen(detail) + 4 + 4 + 4 + 1;
	// Standard checks
	assert_non_null(response);
	assert_int_equal(response->length, htonl(expected_length));

	assert_non_null(response->args[0].value);
	assert_string_equal(message, response->args[2].value + 1);

	free(response->args);
	free(response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_with_no_additional_parms),
		cmocka_unit_test(test_with_additional_parms),
		cmocka_unit_test(test_verify_args_pointers_correct)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
