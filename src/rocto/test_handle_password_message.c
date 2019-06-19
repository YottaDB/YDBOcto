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
#include <endian.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include <openssl/md5.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_ydb_get_s(ydb_buffer_t *varname, int subs_used, ydb_buffer_t *subsarray, ydb_buffer_t *ret_value) {
	ydb_buffer_t *t = mock_ptr_type(ydb_buffer_t*);
	*ret_value = *t;
	return mock_type(int);
}

static void test_valid_input(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	ydb_buffer_t username_subs;
	char *username = "user";
	YDB_STRING_TO_BUFFER(username, &username_subs);
	will_return(__wrap_ydb_get_s, &username_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	ydb_buffer_t user_info_subs;
	// md5 hash of passworduser: 4d45974e13472b5a0be3533de4666414
	char *user_info = "1|user|super|inh|crer|cred|canl|repl|bypassrl|conn|md54d45974e13472b5a0be3533de4666414|valid";
	YDB_STRING_TO_BUFFER(user_info, &user_info_subs);
	will_return(__wrap_ydb_get_s, &user_info_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	ydb_buffer_t salt_subs;
	char *salt = "salt";
	YDB_STRING_TO_BUFFER(salt, &salt_subs);
	will_return(__wrap_ydb_get_s, &salt_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	char *password = "password";
	password_message = make_password_message(username, password, salt);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 0);
	assert_null(err);

	free(password_message);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
