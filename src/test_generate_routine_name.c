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

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "mmrhash.h"

static void test_valid_input_cross_reference(void **state) {
	hash128_state_t hash_state;
	HASH128_STATE_INIT(hash_state, 0);
	char *key1 = "ifembu8r308j243h5g3h84t7yf23h0hewefwefwig08SDGogugWQ)*vw2ef234ASF(C93VC(&TFG2gg";
	char *key2 = "ougoh2408rh2fhe08yh2ti8rhhrguo2r3huocdiWEN23ivuebvuo80AD)C*2o3rblh 08gv#yh8o3vhv7w7";
	char routine_name[MAX_STR_CONST];
	int buf_size = 0;
	int expected_size = 0;
	int max_routine_name_len = 31;	// Reflected in generate_routine_name (hard coded)


	ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
	ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

	buf_size = generate_routine_name(&hash_state, routine_name, MAX_STR_CONST, CrossReference);

	assert_int_equal(buf_size, max_routine_name_len);
}

static void test_valid_input_output_plan(void **state) {
	hash128_state_t hash_state;
	HASH128_STATE_INIT(hash_state, 0);
	char *key1 = "ifembu8r308j243h5g3h84t7yf23h0hewefwefwig08SDGogugWQ)*vw2ef234ASF(C93VC(&TFG2gg";
	char *key2 = "ougoh2408rh2fhe08yh2ti8rhhrguo2r3huocdiWEN23ivuebvuo80AD)C*2o3rblh 08gv#yh8o3vhv7w7";
	char routine_name[MAX_STR_CONST];
	int buf_size = 0;
	int expected_size = 0;
	int max_routine_name_len = 31;	// Reflected in generate_routine_name (hard coded)

	ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
	ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

	buf_size = generate_routine_name(&hash_state, routine_name, MAX_STR_CONST, OutputPlan);

	assert_int_equal(buf_size, max_routine_name_len);
}

static void test_invalid_file_type(void **state) {
	hash128_state_t hash_state;
	HASH128_STATE_INIT(hash_state, 0);
	char *key1 = "ifembu8r308j243h5g3h84t7yf23h0h";
	char *key2 = "ougoh2408rh2fhe08yh2ti8rhhrguo2r3huocdiWEN23";
	char routine_name[MAX_STR_CONST];
	int buf_size = 0;
	int max_routine_name_len = 31;	// Reflected in generate_routine_name (hard coded)

	memset(routine_name, 0, MAX_STR_CONST);

	ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
	ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

	buf_size = generate_routine_name(&hash_state, routine_name, MAX_STR_CONST, 75);

	assert_int_equal(buf_size, -1);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input_cross_reference),
		   cmocka_unit_test(test_valid_input_output_plan),
		   cmocka_unit_test(test_invalid_file_type),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
