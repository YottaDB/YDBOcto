/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <endian.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include <openssl/md5.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

static void test_valid_input(void **state) {
	pid_t pid;
	pid = getpid();

	long long start_time = get_pid_start_time(pid);
	long long start_time2 = get_pid_start_time(pid);
	// Ensure results are consistent
	assert_int_equal(start_time, start_time2);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
