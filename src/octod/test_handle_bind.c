/* Copyright (C) 2018-2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
#include "octod.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_send_message(OctodSession *session, BaseMessage *message) {
	int message_type = mock_type(char);
	assert_true(message_type == message->type);
	return 0;
}

static void test_valid_input(void **state) {
	int result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	OctodSession session;

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
	set("select * from names where firstName = $0", "^session", 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_BindComplete);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, 0);

	result_buffer = get("^session", 3, "0", "bound", "");
	result_buffer->buf_addr[result_buffer->len_used] = '\0';
	assert_string_equal("select * from names where firstName = \"hello\"", result_buffer->buf_addr);
	free(result_buffer->buf_addr);
	free(result_buffer);
	free(bind.parms);
}

static void test_bind_to_non_existent_source(void **state) {
	int result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	OctodSession session;

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

	assert_int_equal(result, -1);
	free(bind.parms);
}

static void test_bind_with_too_many_parms(void **state) {
	int result;
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	OctodSession session;

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

	assert_int_equal(result, -1);
	free(bind.parms);
}

static void test_bind_greater_than_max_str_const(void **state) {
	int result, i;
	char str[MAX_STR_CONST];
	Bind bind;
	ydb_buffer_t session_id, *result_buffer;
	OctodSession session;

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
	set("select * from names where firstName = $0", "^session", 3, "0", "prepared", "sample1");

	will_return(__wrap_send_message, PSQL_ErrorResponse);

	result = handle_bind(&bind, &session);

	assert_int_equal(result, -1);

	free(bind.parms);
}

/// TODO: more testing is needed of this function

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_bind_to_non_existent_source),
		   cmocka_unit_test(test_bind_with_too_many_parms),
		   cmocka_unit_test(test_bind_greater_than_max_str_const)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
