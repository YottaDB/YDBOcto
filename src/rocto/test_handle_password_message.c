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

unsigned int __wrap_get_user_column_value(char *buffer, const unsigned int buf_len, const char *row, const unsigned int row_len,
		enum UserColumns column) {
	strncpy(buffer, mock_type(char*), buf_len);
	return mock_type(unsigned int);
}

int __wrap_md5_to_hex(char *md5_hash, char *hex, unsigned int hex_len) {
	strncpy(hex, mock_type(char*), hex_len);
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

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, strlen(column_value));

	ydb_buffer_t salt_subs;
	char *salt = "salt";
	YDB_STRING_TO_BUFFER(salt, &salt_subs);
	will_return(__wrap_ydb_get_s, &salt_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	// Wrap call in handle_password_message
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	char *password = "password";
	password_message = make_password_message(username, password, salt);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 0);
	assert_null(err);

	free(password_message);
}

static void test_error_not_md5(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char *username = "user";
	char *password = "password";
	char *salt = "salt";

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	password_message = make_password_message(username, password, salt);

	password_message->password = "password";
	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 1);
	assert_non_null(err);

	error_message = format_error_string(&err_buff, ERR_ROCTO_PASSWORD_TYPE, "handle_password_message", "md5");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(password_message);
	free_error_response(err);
}

static void test_error_session_username_lookup(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	ydb_buffer_t username_subs;
	char *username = "user";
	YDB_STRING_TO_BUFFER(username, &username_subs);
	will_return(__wrap_ydb_get_s, &username_subs);
	will_return(__wrap_ydb_get_s, YDB_ERR_LVUNDEF);

	char *salt = "salt";
	char *password = "password";

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	password_message = make_password_message(username, password, salt);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 1);
	assert_non_null(err);

	error_message = format_error_string(&err_buff, ERR_ROCTO_SESSION_LOOKUP, "handle_password_message", "username");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(password_message);
	free_error_response(err);
}

static void test_error_user_info_lookup(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

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
	will_return(__wrap_ydb_get_s, YDB_ERR_LVUNDEF);

	char *salt = "salt";
	char *password = "password";

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	password_message = make_password_message(username, password, salt);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 1);
	assert_non_null(err);

	error_message = format_error_string(&err_buff, ERR_ROCTO_DB_LOOKUP, "handle_password_message", "user info");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(password_message);
	free_error_response(err);
}

static void test_error_hash_lookup(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

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

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, 0);

	char *salt = "salt";
	char *password = "password";

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	password_message = make_password_message(username, password, salt);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 1);
	assert_non_null(err);

	error_message = format_error_string(&err_buff, ERR_ROCTO_COLUMN_VALUE,
			"handle_password_message", "rolpassword (hashed password)");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(password_message);
	free_error_response(err);
}

static void test_error_salt_lookup(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

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

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, strlen(column_value));

	ydb_buffer_t salt_subs;
	char *salt = "salt";
	YDB_STRING_TO_BUFFER(salt, &salt_subs);
	will_return(__wrap_ydb_get_s, &salt_subs);
	will_return(__wrap_ydb_get_s, YDB_ERR_LVUNDEF);

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	char *password = "balugawhales";
	password_message = make_password_message(username, password, salt);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 1);
	assert_non_null(err);

	error_message = format_error_string(&err_buff, ERR_ROCTO_SESSION_LOOKUP,
			"handle_password_message", "temporary salt");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(password_message);
	free_error_response(err);
}

static void test_error_hash_conversion(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

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

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, strlen(column_value));

	ydb_buffer_t salt_subs;
	char *salt = "salt";
	YDB_STRING_TO_BUFFER(salt, &salt_subs);
	will_return(__wrap_ydb_get_s, &salt_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	char *password = "password";
	password_message = make_password_message(username, password, salt);

	// Wrap calls in handle_password_message
	will_return(__wrap_md5_to_hex, "arbitrary");
	will_return(__wrap_md5_to_hex, 1);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 1);
	assert_non_null(err);

	error_message = format_error_string(&err_buff, ERR_ROCTO_HASH_CONVERSION,
			"handle_password_message", "md5 hash", "hexidecimal string");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(password_message);
	free_error_response(err);
}

static void test_error_bad_password(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

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

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, strlen(column_value));

	ydb_buffer_t salt_subs;
	char *salt = "salt";
	YDB_STRING_TO_BUFFER(salt, &salt_subs);
	will_return(__wrap_ydb_get_s, &salt_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	// Wrap calls in make_password_message
	will_return(__wrap_md5_to_hex, "4d45974e13472b5a0be3533de4666414");
	will_return(__wrap_md5_to_hex, 0);
	will_return(__wrap_md5_to_hex, "8e998aaa66bd302e5592df3642c16f78");
	will_return(__wrap_md5_to_hex, 0);

	// Wrap calls in handle_password_message
	will_return(__wrap_md5_to_hex, "arbitrary");
	will_return(__wrap_md5_to_hex, 0);

	char *password = "balugawhales";
	password_message = make_password_message(username, password, salt);

	int result = handle_password_message(password_message, &session, &err);
	assert_int_equal(result, 1);
	assert_non_null(err);

	error_message = format_error_string(&err_buff, ERR_ROCTO_BAD_PASSWORD, "handle_password_message");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(password_message);
	free_error_response(err);
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_error_not_md5),
		   cmocka_unit_test(test_error_session_username_lookup),
		   cmocka_unit_test(test_error_user_info_lookup),
		   cmocka_unit_test(test_error_hash_lookup),
		   cmocka_unit_test(test_error_salt_lookup),
		   cmocka_unit_test(test_error_hash_conversion),
		   cmocka_unit_test(test_error_bad_password),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
