/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

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
	char routine_name[MAX_ROUTINE_LEN + 1];
	memset(routine_name, 0, MAX_ROUTINE_LEN + 1); // zero initialize the array
	int buf_size = 0;
	int status;
	int max_routine_name_len = 31;	// Reflected in generate_routine_name (hard coded)


	ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
	ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

	status = generate_routine_name(&hash_state, routine_name, MAX_ROUTINE_LEN, CrossReference);
	buf_size = strlen(routine_name);

	assert_int_equal(status, 0);
	assert_int_equal(buf_size, max_routine_name_len);
}

static void test_valid_input_output_plan(void **state) {
	hash128_state_t hash_state;
	HASH128_STATE_INIT(hash_state, 0);
	char *key1 = "ifembu8r308j243h5g3h84t7yf23h0hewefwefwig08SDGogugWQ)*vw2ef234ASF(C93VC(&TFG2gg";
	char *key2 = "ougoh2408rh2fhe08yh2ti8rhhrguo2r3huocdiWEN23ivuebvuo80AD)C*2o3rblh 08gv#yh8o3vhv7w7";
	char routine_name[MAX_ROUTINE_LEN + 1];
	memset(routine_name, 0, MAX_ROUTINE_LEN + 1); // zero initialize the array
	int buf_size = 0;
	int status;
	int max_routine_name_len = 31;	// Reflected in generate_routine_name (hard coded)

	ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
	ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

	status = generate_routine_name(&hash_state, routine_name, MAX_ROUTINE_LEN, OutputPlan);
	buf_size = strlen(routine_name);

	assert_int_equal(status, 0);
	assert_int_equal(buf_size, max_routine_name_len);
}

static void test_valid_input_trigger_plan(void **state) {
	hash128_state_t hash_state;
	HASH128_STATE_INIT(hash_state, 0);
	char *key1 = "ifembu8r308j243h5g3h84t7yf23h0hewefwefwig08SDGogugWQ)*vw2ef234ASF(C93VC(&TFG2gg";
	char *key2 = "ougoh2408rh2fhe08yh2ti8rhhrguo2r3huocdiWEN23ivuebvuo80AD)C*2o3rblh 08gv#yh8o3vhv7w7";
	char routine_name[MAX_TRIGGER_LEN + 1];
	memset(routine_name, 0, MAX_TRIGGER_LEN+ 1); // zero initialize the array
	int buf_size = 0;
	int status;
	int max_trigger_name_len = 28;	// Reflected in generate_routine_name (hard coded)

	ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
	ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

	status = generate_routine_name(&hash_state, routine_name, MAX_TRIGGER_LEN, YDBTrigger);
	buf_size = strlen(routine_name);

	assert_int_equal(status, 0);
	assert_int_equal(buf_size, max_trigger_name_len);
}

static void test_invalid_file_type(void **state) {
	hash128_state_t hash_state;
	HASH128_STATE_INIT(hash_state, 0);
	char *key1 = "ifembu8r308j243h5g3h84t7yf23h0h";
	char *key2 = "ougoh2408rh2fhe08yh2ti8rhhrguo2r3huocdiWEN23";
	char routine_name[MAX_ROUTINE_LEN + 1];
	int status;
	memset(routine_name, 0, MAX_ROUTINE_LEN + 1); // zero initialize the array

	ydb_mmrhash_128_ingest(&hash_state, (void*)key1, strlen(key1));
	ydb_mmrhash_128_ingest(&hash_state, (void*)key2, strlen(key2));

	status = generate_routine_name(&hash_state, routine_name, MAX_ROUTINE_LEN, 75);

	assert_int_equal(status, 1);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input_cross_reference),
		   cmocka_unit_test(test_valid_input_output_plan),
		   cmocka_unit_test(test_valid_input_trigger_plan),
		   cmocka_unit_test(test_invalid_file_type),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
