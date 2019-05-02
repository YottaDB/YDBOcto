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
	PortalSuspended *response;
	PortalSuspended *received_response;
	ErrorResponse *err = NULL;

	int expected_length = sizeof(unsigned int);
	response = make_portal_suspended();
	received_response = read_portal_suspended((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_null(err);
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_PortalSuspended);
	assert_int_equal(received_response->length, expected_length);

	free(response);
	free(received_response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
