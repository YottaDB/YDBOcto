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
#include <errno.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

int __wrap_read_bytes(RoctoSession *session, char *message, int buffer_size, int bytes_to_read) {
	int expected_return = mock_type(int);
	return expected_return;
}

static void test_valid_input(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	will_return(__wrap_read_bytes, 0);
	will_return(__wrap_read_bytes, 0);

	result = read_message(NULL, (char*)buffer, 0);

	assert_non_null(result);

	free(buffer);
}

static void test_failed_first_read(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	// Test first read fails
	will_return(__wrap_read_bytes, 1);

	result = read_message(NULL, (char*)buffer, 0);

	assert_null(result);

	free(buffer);
}

static void test_failed_second_read(void **state) {
	BaseMessage *result;
	BaseMessage *buffer = malloc(sizeof(BaseMessage));
	memset(buffer, 'X', sizeof(BaseMessage));

	// Test second read fails
	will_return(__wrap_read_bytes, 0);
	will_return(__wrap_read_bytes, 1);

	result = read_message(NULL, (char*)buffer, 0);

	assert_null(result);

	free(buffer);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_failed_first_read),
		cmocka_unit_test(test_failed_second_read),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
