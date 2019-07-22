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

/*
int __wrap_ydb_set_s(ydb_buffer_t *varname, int subs_used, ydb_buffer_t *subsarray, ydb_buffer_t *value) {
	if (0 == strncmp(varname->buf_addr, "$ZGBLDIR", varname->len_used)) {
		return 0;
	}
	return mock_type(int);
}
*/

static void test_valid_input(void **state) {
	RoctoSession session;
	ErrorResponse *err = NULL;
	ydb_buffer_t session_id;
	YDB_STRING_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	int expected_length = sizeof(unsigned int) + sizeof(pid_t) + sizeof(int);
	pid_t pid = 1111;
	int secret_key = 42;

	BackendKeyData *backend_key_data = make_backend_key_data(secret_key, pid);

	assert_null(err);
	assert_non_null(backend_key_data);
	assert_int_equal(backend_key_data->length, htonl(expected_length));
	assert_int_equal(backend_key_data->pid, htonl(pid));
	assert_int_equal(backend_key_data->secret_key, htonl(secret_key));

	free(backend_key_data);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
