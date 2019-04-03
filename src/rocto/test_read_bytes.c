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

int __wrap_recv(int *socfd, void *buf, size_t len, int flags) {
	int expected_return = mock_type(int);
	errno = mock_type(int);
	return expected_return;
}

static void test_valid_input(void **state) {
	char *buffer;
	int buffer_size = 0, bytes_to_read = 0;
	int rt = 1;
	RoctoSession session;

	// Bytes match buffer size
	buffer_size = 10;
	bytes_to_read = 10;

	// Initialize relevant variables
	buffer = malloc(sizeof(char) * buffer_size);
	memset(buffer, 0, buffer_size);
	session.connection_fd = 0;

	will_return(__wrap_recv, 10);	// Number of bytes read
	will_return(__wrap_recv, 0);	// No error

	rt = read_bytes(&session, buffer, buffer_size, bytes_to_read);

	assert_int_equal(rt, 0);

	free(buffer);
}

static void test_read_too_large(void **state) {
}

static void test_invalid_read_size(void **state) {
}

static void test_recv_interrupted(void **state) {
}

static void test_recv_connection_reset(void **state) {
}

static void test_socket_closed(void **state) {
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
