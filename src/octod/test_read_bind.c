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

#include "message_formats.h"

static void test_valid_input(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	// 5 is the size of message code (1) and length (4)
	int message_length = 5 + strlen(dest) + strlen(source) + sizeof(short int) * 3;
	BaseMessage *test_data = (BaseMessage*)malloc(message_length);
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
	Bind *bind = read_bind(test_data);

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
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
