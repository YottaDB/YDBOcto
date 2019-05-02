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
	ErrorResponse *err = NULL;

	// Populate base message
        BaseMessage *test_data = (BaseMessage*)malloc(sizeof(BaseMessage));
	test_data->type = PSQL_Flush;
	test_data->length = htonl(sizeof(unsigned int));

	// The actual test
	Flush *flush = read_flush(test_data, &err);

	// Standard checks
	assert_non_null(flush);
	assert_null(err);

	free(test_data);
	free(flush);
}

static void test_invalid_type(void **state) {
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	const char *error_message;
	err_buff.offset = 0;

	// Populate base message
        BaseMessage *test_data = (BaseMessage*)malloc(sizeof(BaseMessage));
	test_data->type = 'X';
	test_data->length = htonl(sizeof(unsigned int));

	// The actual test
	Flush *flush = read_flush(test_data, &err);

	// Standard checks
	assert_null(flush);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Flush", test_data->type, PSQL_Flush);
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_invalid_type),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
