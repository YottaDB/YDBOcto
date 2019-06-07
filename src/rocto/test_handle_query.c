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

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_send_message(RoctoSession *session, BaseMessage *message) {
	int message_type = mock_type(char);
	assert_true(message_type == message->type);
	return 0;
}

int __wrap_run_query(char *query, void (*callback)(PhysicalPlan *, int, void *), void *parms) {
	int expected_return = mock_type(int);
	eof_hit = TRUE;
	return expected_return;
}

static void test_valid_input(void **state) {
	Query *query;
	RoctoSession session;
	char *query_text = "SELECT * FROM names WHERE firstName = \"Acid\";", *c;
	int result;

	query = (Query*)malloc(sizeof(Query) + strlen(query_text) + 1);
	c = query->data;
	memcpy(c, query_text, strlen(query_text));
	c += strlen(query_text);
	*c++ = '\0';
	query->type = PSQL_Query;
	query->query = query->data;

	will_return(__wrap_run_query, TRUE);
	result = handle_query(query, &session);

	assert_int_equal(result, 0);
}

static void test_bad_sql(void **state) {
	Query *query;
	RoctoSession session;
	char *query_text = "SELECT * FOREVERMORE names WHERE firstName = \"Acid\";", *c;
	int result;

	query = (Query*)malloc(sizeof(Query) + strlen(query_text) + 1);
	c = query->data;
	memcpy(c, query_text, strlen(query_text));
	c += strlen(query_text);
	*c++ = '\0';
	query->type = PSQL_Query;
	query->query = query->data;

	will_return(__wrap_send_message, PSQL_ErrorResponse);
	will_return(__wrap_run_query, FALSE);
	result = handle_query(query, &session);

	assert_int_equal(result, 0);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_bad_sql)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
