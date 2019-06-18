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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

static void test_valid_input(void **state) {
	PasswordMessage *password_message;
	char *user = "user";
	char *password = "password";
	char *salt = "salt";

	password_message = make_password_message(user, password, salt);
	assert_non_null(password_message);
	// MD5 of "passworduser" == 4d45974e13472b5a0be3533de4666414
	// MD5 of "4d45974e13472b5a0be3533de4666414salt" == 8e998aaa66bd302e5592df3642c16f78
	assert_string_equal(password_message->password, "md58e998aaa66bd302e5592df3642c16f78");
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
