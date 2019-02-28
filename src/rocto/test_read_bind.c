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
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	// 4 is the size of length (4), 2 null chars
	int message_length = 4 + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 1);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_null(bind->parm_format_codes);
	assert_null(bind->parms);
	assert_null(bind->result_col_format_codes);
	assert_null(err);
}

static void test_input_to_short(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	// 4 is the size of length (4)
	int message_length = 4 + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 1);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length/2);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);
}

static void test_input_to_long(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	// 4 is the size of length (4); we add 50 to make the input too long
	int message_length = 4 + strlen(dest) + strlen(source) + 2  + sizeof(short int) * 3 + 500;
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 1);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_null(bind->parm_format_codes);
	assert_null(bind->parms);
	assert_null(bind->result_col_format_codes);
	// Verify an error was issued
	assert_non_null(err);
}

static void test_no_null_terminators_on_dest(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	// 4 is the size of length (4)
	int message_length = 4 + strlen(dest);
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 5);
	memset(test_data, 0xdeadbeef, message_length + 5);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure the return error mentions "dest"
	assert_string_equal("bind destination missing null termination",
	  err->args[2].value + 1);
}

static void test_no_null_terminators_on_source(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	// 4 is the size of length (4)
	int message_length = 4 + strlen(dest) + strlen(source);
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 5);
	memset(test_data, 0xdeadbeef, message_length + 5);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure the return error mentions "dest"
	assert_string_equal("bind SQL missing null termination",
	  err->args[2].value + 1);
}

static void test_missing_parameter_types(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(10);
	// 4 is the size of length (4)
	int message_length = 4 + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 1;
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 1);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure the return error mentions "dest"
	assert_string_equal("bind incomplete/missing parameter format code list",
	  err->args[2].value + 1);
}

static void test_missing_parameters(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(10);
	// 4 is the size of length (4)
	int message_length = 4 + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 2;
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 1);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure the return error mentions "dest"
	assert_string_equal("bind incomplete/missing parameters",
	  err->args[2].value + 1);
}

static void test_missing_result_col_format_codes(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(10);
	// 4 is the size of length (4)
	int message_length = 4 + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + 1);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure the return error mentions "dest"
	assert_string_equal("bind missing/incomplete result format codes",
	  err->args[2].value + 1);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_input_to_short),
		cmocka_unit_test(test_input_to_long),
		cmocka_unit_test(test_no_null_terminators_on_dest),
		cmocka_unit_test(test_no_null_terminators_on_source),
		cmocka_unit_test(test_missing_parameter_types),
		cmocka_unit_test(test_missing_parameters),
		cmocka_unit_test(test_missing_result_col_format_codes)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
