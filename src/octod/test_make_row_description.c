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

#include "octod.h"
#include "message_formats.h"

static void test_null_input(void **state) {
	RowDescription *response = make_row_description(NULL, 0);
	int expected_length = 6;
	// Standard checks
	assert_non_null(response);
	assert_int_equal(response->length, htonl(expected_length));

	free(response);
}

static void test_one_parms(void **state) {
	int num_parms = 1;
	RowDescriptionParm parms[num_parms];

	memset(parms, 0, sizeof(RowDescriptionParm) * num_parms);
	parms[0].name = "helloWorld";
	
	RowDescription *response = make_row_description(parms, 1);
	// RowDescription + RowDescriptionParms + string in each parm + null char
	int expected_length = 6 + (sizeof(RowDescriptionParm) - sizeof(char*)) * num_parms
		+ strlen(parms[0].name) + 1;
	// Standard checks
	assert_non_null(response);
	assert_int_equal(ntohl(response->length), expected_length);

	free(response);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_null_input),
		cmocka_unit_test(test_one_parms)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
