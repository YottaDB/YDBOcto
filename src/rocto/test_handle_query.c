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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_send_message(RoctoSession *session, BaseMessage *message) {
	int32_t message_type = mock_type(char);
	assert_true(message_type == message->type);

	return 0;
}

int __wrap_run_query(callback_fnptr_t callback, void *parms, boolean_t send_row_description, ParseContext *parse_context) {
	int32_t expected_return = mock_type(int);
	eof_hit = EOF_CTRLD;

	return expected_return;
}

static void test_valid_input(void **state) {
	Query *	     query;
	RoctoSession session;
	char *	     query_text = "SELECT * FROM names WHERE firstName = \"Acid\";";
	int	     query_length = strlen(query_text);
	int32_t	     result;

	query = (Query *)malloc(sizeof(Query) + query_length + 1);
	query->type = PSQL_Query;
	query->length = sizeof(unsigned int) + query_length + 1;
	strncpy(query->data, query_text, query_length);
	query->data[query_length] = '\0';
	query->query = query->data;

	will_return(__wrap_run_query, 0);
	result = handle_query(query, &session);

	free(query);
	assert_int_equal(result, 0);
}

static void test_query_length_equals_zero(void **state) {
	Query *	     query;
	RoctoSession session;
	char *	     query_text = "";
	int	     query_length = strlen(query_text);
	int32_t	     result;

	query = (Query *)malloc(sizeof(Query) + query_length);
	query->type = PSQL_Query;
	query->length = sizeof(unsigned int) + query_length;
	strncpy(query->data, query_text, query_length);
	query->query = query->data;

	will_return(__wrap_send_message, PSQL_EmptyQueryResponse);
	result = handle_query(query, &session);

	free(query);
	assert_int_equal(result, 1);
}

static void test_query_length_greater_than_max(void **state) {
	// change global variable's value to a small number for testing
	cur_input_max = 2;

	Query *	     query;
	RoctoSession session;
	char *	     query_text = "SELECT * FROM names WHERE firstName = \"Acid\";";
	int	     query_length = strlen(query_text);
	int32_t	     result;

	query = (Query *)malloc(sizeof(Query) + query_length + 1);
	query->type = PSQL_Query;
	query->length = sizeof(unsigned int) + query_length + 1;
	strncpy(query->data, query_text, query_length);
	query->data[query_length] = '\0';
	query->query = query->data;

	assert(query->length > cur_input_max);

	will_return(__wrap_run_query, 0);
	result = handle_query(query, &session);

	// change global variable's value back to what it should be
	cur_input_max = INIT_QUERY_SIZE;
	assert_int_equal(result, 0);
}

static void test_run_query_result_equals_negative_one(void **state) {
	Query *	     query;
	RoctoSession session;
	char *	     query_text = "SELECT * FROM names WHERE firstName = \"Acid\";";
	int	     query_length = strlen(query_text);
	int32_t	     result;

	query = (Query *)malloc(sizeof(Query) + query_length + 1);
	query->type = PSQL_Query;
	query->length = sizeof(unsigned int) + query_length + 1;
	strncpy(query->data, query_text, query_length);
	query->data[query_length] = '\0';
	query->query = query->data;

	will_return(__wrap_run_query, -1);
	result = handle_query(query, &session);

	assert_int_equal(result, -1);
}

// Also cannot equal -1, as that would trigger the same thing as the test above
static void test_run_query_result_does_not_equal_zero(void **state) {
	Query *	     query;
	RoctoSession session;
	char *	     query_text = "SELECT * FROM names WHERE firstName = \"Acid\";";
	int	     query_length = strlen(query_text);
	int32_t	     result;

	query = (Query *)malloc(sizeof(Query) + query_length + 1);
	query->type = PSQL_Query;
	query->length = sizeof(unsigned int) + query_length + 1;
	strncpy(query->data, query_text, query_length);
	query->data[query_length] = '\0';
	query->query = query->data;

	will_return(__wrap_run_query, 1);
	result = handle_query(query, &session);

	assert_int_equal(result, 1);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input),
	    cmocka_unit_test(test_query_length_equals_zero),
	    cmocka_unit_test(test_query_length_greater_than_max),
	    cmocka_unit_test(test_run_query_result_equals_negative_one),
	    cmocka_unit_test(test_run_query_result_does_not_equal_zero),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
