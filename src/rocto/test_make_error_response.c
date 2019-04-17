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

static void test_with_one_parm(void **state) {
	ErrorResponse *received_response = NULL;
	ErrorResponse *response = NULL;
	ErrorResponse *err = NULL;
	char *message = "Seems OK to me man";
	char *detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR,
			PSQL_Code_Success,
			message,
			1, &a);

	// Expected length is each string + null terminating bytes + response arg type + length + final null byte
	int expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success])
		+ strlen(message) + strlen(detail) + (4 * sizeof(char))
		+ sizeof(unsigned int) + sizeof(unsigned int) + sizeof(char);

	received_response = read_error_response((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_null(err);
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_with_no_additional_parms(void **state) {
	ErrorResponse *received_response = NULL;
	ErrorResponse *response = NULL;
	ErrorResponse *err = NULL;
	char *message = "Seems OK to me man";
	char *detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR,
			PSQL_Code_Success,
			message, 0);

	// Expected length is each string + null terminating bytes + response arg type + length + final null byte
	int expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success]) + strlen(message)
		+ (3 * sizeof(char)) + (3 * sizeof(char)) + sizeof(unsigned int) + sizeof(char);

	received_response = read_error_response((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_null(err);
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_with_additional_parms(void **state) {
	ErrorResponse *received_response = NULL;
	ErrorResponse *response = NULL;
	ErrorResponse *err = NULL;
	char *message = "Seems OK to me man";
	char *detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR,
			PSQL_Code_Success,
			message, 10, &a, &a, &a,
			&a, &a, &a, &a, &a, &a, &a);

	// Total of 13 items: 3 required, 10 optional. Each includes a null terminator and type code. Include length and type also.
	int expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success]) + strlen(message)
		+ (10 * strlen(detail)) + (13 * sizeof(char)) + (13 * sizeof(char)) + sizeof(unsigned int) + sizeof(char);

	received_response = read_error_response((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_null(err);
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_verify_args_pointers_correct(void **state) {
	ErrorResponse *received_response = NULL;
	ErrorResponse *response = NULL;
	ErrorResponse *err = NULL;
	char *message = "Seems OK to me man";
	char *detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR,
			PSQL_Code_Success,
			message, 10, &a, &a, &a,
			&a, &a, &a, &a, &a, &a, &a);

	// Total of 13 args: 3 required, 10 optional.
	int num_args = 3 + 10;
	// Each arg includes a null terminator and type code. Count length and type of ErrorResponse as usual.
	int expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
		+ strlen(psql_sqlstate_codes_str[PSQL_Code_Success]) + strlen(message)
		+ (10 * strlen(detail)) + (num_args * sizeof(char)) + (num_args * sizeof(char)) + sizeof(unsigned int) + sizeof(char);

	received_response = read_error_response((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_null(err);
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	int i = 0;
	for (i = 0; i < num_args; i++) {
		assert_non_null(received_response->args[i].value);
		switch (i) {
			case 0:
				assert_int_equal(received_response->args[0].type, PSQL_Error_SEVERITY);
				assert_string_equal(psql_error_severity_str[PSQL_Error_ERROR], received_response->args[0].value);
				break;
			case 1:
				assert_int_equal(received_response->args[1].type, PSQL_Error_Code);
				assert_string_equal(psql_sqlstate_codes_str[PSQL_Code_Success], received_response->args[1].value);
				break;
			case 2:
				assert_int_equal(received_response->args[2].type, PSQL_Error_Message);
				assert_string_equal(message, received_response->args[2].value);
				break;
			default:
				assert_int_equal(received_response->args[i].type, PSQL_Error_Detail);
				assert_string_equal(detail, received_response->args[i].value);
				break;
		}
	}

	free_error_response(response);
	free_error_response(received_response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_with_one_parm),
		cmocka_unit_test(test_with_no_additional_parms),
		cmocka_unit_test(test_with_additional_parms),
		cmocka_unit_test(test_verify_args_pointers_correct)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
