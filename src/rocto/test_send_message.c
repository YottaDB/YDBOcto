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
	int32_t expected_return = mock_type(int);
	return expected_return;
}

static void test_valid_input(void **state) {
	int32_t	     rt = 1;
	BaseMessage  message;
	RoctoSession session;

	// Initialize relevant variables
	message.type = 'S';
	message.length = sizeof(uint32_t);

	will_return(__wrap_send_bytes, 0); // Success

	rt = send_message(&session, &message); // Count type indicator

	assert_int_equal(rt, 0);
}

static void test_send_bytes_failed(void **state) {
	int32_t	     rt = 1;
	uint32_t     length = sizeof(BaseMessage);
	BaseMessage  message;
	RoctoSession session;

	will_return(__wrap_send_bytes, 1); // Failure

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
