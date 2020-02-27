/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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

static void test_no_asserts_in_release_builds(void **state) {
	assert(0);
}

int main(void) {
	const struct CMUnitTest tests[] = {
			cmocka_unit_test(test_no_asserts_in_release_builds),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
