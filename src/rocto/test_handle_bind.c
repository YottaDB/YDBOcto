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

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_send_message(RoctoSession *session, BaseMessage *message) {
	int32_t message_type = mock_type(char);
	assert_true(message_type == message->type);
	return 0;
}

static void test_one_parm_text_format(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 0;
	bind.parm_format_codes = NULL;
	bind.num_parms = 1;
	bind.parms = (BindParm*)malloc(1 * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select * from names where firstName = $0", config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	// result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select * from names where firstName = \"hello\"", result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_two_parm_text_format(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 0;
	bind.parm_format_codes = NULL;
	bind.num_parms = 2;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.parms[1].value = "world";
	bind.parms[1].length = strlen(bind.parms[1].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 else firstName end from names",
			config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);
	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"hello\" then \"world\" else firstName end from names",
			result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_four_parm_text_format(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 0;
	bind.parm_format_codes = NULL;
	bind.num_parms = 4;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.parms[1].value = "world";
	bind.parms[1].length = strlen(bind.parms[1].value);
	bind.parms[2].value = "crunchy";
	bind.parms[2].length = strlen(bind.parms[2].value);
	bind.parms[3].value = "pickles";
	bind.parms[3].length = strlen(bind.parms[3].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 when $2 then $3 else firstName end from names",
			config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);
	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"hello\" then \"world\" when \"crunchy\" then \"pickles\" else firstName end from names",
			result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}


static void test_one_parm_binary_format(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char value;
	value = 7;
	int16_t codes[1] = {1};
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 1;
	bind.parm_format_codes = codes;
	bind.num_parms = 1;
	bind.parms = (BindParm*)malloc(1 * sizeof(BindParm));
	bind.parms[0].value = &value;
	bind.parms[0].length = sizeof(char);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select * from names where firstName = $0", config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select * from names where firstName = \"7\"", result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_two_parm_binary_format(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;
	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char uuid[16];
	int32_t i = 0;
	for (i = 0; i < 16; i++) {
		uuid[i] = i;
	}
	int16_t value;
	value = htons(12345);
	int16_t codes[1] = {1};
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 1;
	bind.parm_format_codes = codes;
	bind.num_parms = 2;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = &value;
	bind.parms[0].length = sizeof(int16_t);
	bind.parms[1].value = uuid;
	bind.parms[1].length = sizeof(uuid);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 else firstName end from names", config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"12345\" then \"00010203-0405-0607-0809-0a0b0c0d0e0f\" else firstName end from names",
			result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_four_parm_binary_format(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	int16_t value1, value2, value3, value4;
	value1 = htons(12345);
	value2 = htons(54321);
	value3 = htons(678);
	value4 = htons(876);
	int16_t codes[1] = {1};
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 1;
	bind.parm_format_codes = codes;
	bind.num_parms = 4;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = &value1;
	bind.parms[0].length = sizeof(int16_t);
	bind.parms[1].value = &value2;
	bind.parms[1].length = sizeof(int16_t);
	bind.parms[2].value = &value3;
	bind.parms[2].length = sizeof(int16_t);
	bind.parms[3].value = &value4;
	bind.parms[3].length = sizeof(int16_t);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 when $2 then $3 else firstName end from names",
			config->global_names.session, 3, "0", "prepared", "sample1");
	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"12345\" then \"54321\" when \"678\" then \"876\" else firstName end from names",
			result_buffer->buf_addr);

	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_one_parm_one_code_text(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	int16_t codes[1] = {0};
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 1;
	bind.parm_format_codes = codes;
	bind.num_parms = 1;
	bind.parms = (BindParm*)malloc(1 * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select * from names where firstName = $0", config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select * from names where firstName = \"hello\"", result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_two_parm_one_code_text(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	int16_t codes[1] = {0};
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 1;
	bind.parm_format_codes = codes;
	bind.num_parms = 2;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.parms[1].value = "world";
	bind.parms[1].length = strlen(bind.parms[1].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 else firstName end from names",
			config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);
	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"hello\" then \"world\" else firstName end from names",
			result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_four_parm_one_code_text(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	int16_t codes[1] = {0};
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 1;
	bind.parm_format_codes = codes;
	bind.num_parms = 4;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.parms[1].value = "world";
	bind.parms[1].length = strlen(bind.parms[1].value);
	bind.parms[2].value = "crunchy";
	bind.parms[2].length = strlen(bind.parms[2].value);
	bind.parms[3].value = "pickles";
	bind.parms[3].length = strlen(bind.parms[3].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 when $2 then $3 else firstName end from names",
			config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);
	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"hello\" then \"world\" when \"crunchy\" then \"pickles\" else firstName end from names",
			result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_two_parm_two_code(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	int16_t codes[2] = {0, 1};
	int64_t value;
	value = htobe64(12345);
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 2;
	bind.parm_format_codes = codes;
	bind.num_parms = 2;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.parms[1].value = &value;
	bind.parms[1].length = sizeof(int64_t);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 else firstName end from names",
			config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);
	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"hello\" then \"12345\" else firstName end from names",
			result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_four_parm_four_code(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	int16_t codes[4] = {0, 1, 0, 1};
	int16_t value1;
	int32_t value2;
	value1 = htons(12345);
	value2 = htonl(54321);
	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 4;
	bind.parm_format_codes = codes;
	bind.num_parms = 4;
	bind.parms = (BindParm*)malloc(bind.num_parms * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.parms[1].value = &value1;
	bind.parms[1].length = sizeof(int16_t);
	bind.parms[2].value = "world";
	bind.parms[2].length = strlen(bind.parms[2].value);
	bind.parms[3].value = &value2;
	bind.parms[3].length = sizeof(int32_t);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select firstName, case firstName when $0 then $1 when $2 then $3 else firstName end from names",
			config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);
	assert_int_equal(result, 0);

	result_buffer = get(config->global_names.session, 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select firstName, case firstName when \"hello\" then \"12345\" when \"world\" then \"54321\" else firstName end from names",
			result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_bind_to_non_existent_source(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	bind.dest = "";
	bind.source = "sampleDoesNotExist";
	bind.num_parm_format_codes = 0;
	bind.parm_format_codes = NULL;
	bind.num_parms = 1;
	bind.parms = (BindParm*)malloc(1 * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	will_return(__wrap_send_message, PSQL_ErrorResponse);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 1);
	free(bind.parms);
}

static void test_bind_with_too_many_parms(void **state) {
	int32_t result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	bind.dest = "";
	bind.source = "sampleDoesNotExist";
	bind.num_parm_format_codes = 0;
	bind.parm_format_codes = NULL;
	bind.num_parms = 2;
	bind.parms = (BindParm*)malloc(2 * sizeof(BindParm));
	bind.parms[0].value = "hello";
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.parms[1].value = "hello";
	bind.parms[1].length = strlen(bind.parms[0].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	will_return(__wrap_send_message, PSQL_ErrorResponse);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 1);
	free(bind.parms);
}

static void test_bind_greater_than_max_str_const(void **state) {
	int32_t result, i;
	char str[MAX_STR_CONST];
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	RoctoSession session;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	for(i = 0; i < MAX_STR_CONST; i++) {
		str[i] = 'a';
	}

	bind.dest = "";
	bind.source = "sample1";
	bind.num_parm_format_codes = 0;
	bind.parm_format_codes = NULL;
	bind.num_parms = 1;
	bind.parms = (BindParm*)malloc(1 * sizeof(BindParm));
	bind.parms[0].value = str;
	bind.parms[0].length = strlen(bind.parms[0].value);
	bind.num_result_col_format_codes = 0;
	bind.result_col_format_codes = NULL;

	// Store a value in ^session(id, "prepared", "sample1")
	set("select * from names where firstName = $0", config->global_names.session, 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_ErrorResponse);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 1);

	free(bind.parms);
}

/// TODO: more testing is needed of this function

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_one_parm_text_format),
		   cmocka_unit_test(test_two_parm_text_format),
		   cmocka_unit_test(test_four_parm_text_format),
		   cmocka_unit_test(test_one_parm_binary_format),
		   cmocka_unit_test(test_two_parm_binary_format),
		   cmocka_unit_test(test_four_parm_binary_format),
		   cmocka_unit_test(test_one_parm_one_code_text),
		   cmocka_unit_test(test_two_parm_one_code_text),
		   cmocka_unit_test(test_four_parm_one_code_text),
		   cmocka_unit_test(test_two_parm_two_code),
		   cmocka_unit_test(test_four_parm_four_code),
		   cmocka_unit_test(test_bind_to_non_existent_source),
		   cmocka_unit_test(test_bind_with_too_many_parms),
		   cmocka_unit_test(test_bind_greater_than_max_str_const)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
