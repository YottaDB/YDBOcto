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

#include <openssl/ssl.h>

#include "rocto.h"
#include "message_formats.h"

int __wrap_send_bytes(RoctoSession *session, char *message, size_t length) {
	int expected_return = mock_type(int);
	return expected_return;
}

static void test_valid_input(void **state) {
	int rt = 1;
	BaseMessage message;
	RoctoSession session;

	// Initialize relevant variables
	message.type = 'S';
	message.length = sizeof(unsigned int);

	will_return(__wrap_send_bytes, 0);	// Success

	rt = send_message(&session, &message);	// Count type indicator

	assert_int_equal(rt, 0);
}

static void test_send_bytes_failed(void **state) {
	int rt = 1;
	unsigned int length = sizeof(BaseMessage);
	BaseMessage message;
	RoctoSession session;

	will_return(__wrap_send_bytes, 1);		// Failure

	rt = send_message(&session, &message);

	assert_int_equal(rt, 1);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_send_bytes_failed),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
