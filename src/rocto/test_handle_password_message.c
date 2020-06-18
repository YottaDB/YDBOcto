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
#include <endian.h>
#include <unistd.h>
#include <libgen.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include <openssl/md5.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_ydb_get_s(ydb_buffer_t *varname, int32_t subs_used, ydb_buffer_t *subsarray, ydb_buffer_t *ret_value) {
	if (0 == strncmp(varname->buf_addr, "$ZGBLDIR", varname->len_used)) {
		return 0;
	} else if (0 == strncmp(varname->buf_addr, "$zroutines", varname->len_used)){
		char *ydb_chset, *ydb_routines, *src_path;
		char exe_path[OCTO_PATH_MAX];
		ssize_t exe_path_len;

		exe_path_len = readlink("/proc/self/exe", exe_path, OCTO_PATH_MAX);
		if ((-1 != exe_path_len) && (OCTO_PATH_MAX > exe_path_len)) {
			exe_path[exe_path_len] = '\0';		// readlink() doesn't add a null terminator per man page
			src_path = dirname(exe_path);
		}
		ydb_chset = getenv("ydb_chset");
		ydb_routines = getenv("ydb_routines");
		if ((NULL != ydb_chset) && (0 == strncmp(ydb_chset, "UTF-8", INT16_TO_STRING_MAX))) {
			if (NULL != strstr(ydb_routines, ". ")) {
				// Strip current directory from $ydb_routines if found
				setenv("ydb_routines", &ydb_routines[2], TRUE);
			}
			if (NULL != src_path)
				ret_value->len_used = snprintf(ret_value->buf_addr, OCTO_PATH_MAX, "%s/utf8", src_path);
		} else {
			if (NULL != src_path)
				ret_value->len_used = snprintf(ret_value->buf_addr, OCTO_PATH_MAX, "%s", src_path);
		}
		return 0;
	}

	ydb_buffer_t *t = mock_ptr_type(ydb_buffer_t*);
	*ret_value = *t;
	return mock_type(int);
}

uint32_t __wrap_get_user_column_value(char *buffer, const uint32_t buf_len, const char *row, const uint32_t row_len,
		enum UserColumns column) {
	strncpy(buffer, mock_type(char*), buf_len);
	return mock_type(uint32_t);
}

int32_t __wrap_octo_log(int line, char *file, enum VERBOSITY_LEVEL level, enum SEVERITY_LEVEL severity, enum ERROR error, ...) {
	return mock_type(int32_t);
}

// Creates a basic StartupMessage with only a single parameter set for testing purposes.
static StartupMessage *make_startup_message(char *parm_name, char *parm_value) {
	StartupMessage	*ret;
	uint32_t	data_len = 0;
	uint32_t	name_len;
	uint32_t	value_len = 0;
	char		*c;

	// Get length of parameter name and value
	name_len = strlen(parm_name) + 1;
	value_len += strlen(parm_value) + 1;
	data_len = name_len + value_len;

	ret = (StartupMessage*)malloc(sizeof(StartupMessage) + data_len);

	// Set length and protocol version
	ret->length = sizeof(uint32_t) + sizeof(int) + data_len;
	ret->protocol_version = 0x00030000;
	ret->num_parameters = 1;
	// Populate data section
	c = ret->data;
	memcpy(c, parm_name, name_len);
	c += name_len;
	memcpy(c, parm_value, value_len);

	// Populate parameter(s)
	ret->parameters = (StartupMessageParm*)malloc(sizeof(StartupMessageParm) * ret->num_parameters);
	ret->parameters[0].name = parm_name;
	ret->parameters[0].value = parm_value;

	return ret;
}

// Make function to simulate client transmission of password_message.
static PasswordMessage *make_password_message(char *user, char *password, char *salt) {
	#define HEX_HASH_LEN MD5_DIGEST_LENGTH * 2 + 1	/* count null */
	#define MD5_PASSWORD_LEN HEX_HASH_LEN + 3	/* Add "md5" prefix to hex hash */

	PasswordMessage *ret;
	int32_t length = 0, result;
	unsigned char hash_buf[MAX_STR_CONST];
	char hex_hash[HEX_HASH_LEN];

	// Rather than have special logic for the NULL, just use an empty string
	if (password == NULL) {
		password = "";
	}

	// Concatenate password and user
	result = sprintf((char*)hash_buf, "%s%s", password, user);
	if (0 > result) {
		return NULL;
	}
	// Hash password and user
	MD5(hash_buf, strlen((char*)hash_buf), hash_buf);
	// Convert hash to hex string
	result = md5_to_hex(hash_buf, hex_hash, HEX_HASH_LEN);
	if (0 != result) {
		return NULL;
	}

	// Concatenate password/user hash with salt
	result = snprintf((char*)hash_buf, HEX_HASH_LEN + 4, "%s%s", hex_hash, salt);	// Exclude "md5" prefix
	if (0 > result) {
		return NULL;
	}
	// Hash password/user hash with salt
	MD5(hash_buf, strlen((char*)hash_buf), hash_buf);

	// Convert hash to hex string
	result = md5_to_hex(hash_buf, hex_hash, HEX_HASH_LEN);
	if (0 != result) {
		return NULL;
	}

	// Add "md5" prefix to hex hash for transmission
	char md5_password[MD5_PASSWORD_LEN];
	result = snprintf(md5_password, MD5_PASSWORD_LEN, "%s%s", "md5", hex_hash);
	if (0 > result) {
		return NULL;
	}

	length += sizeof(uint32_t);
	length += MD5_PASSWORD_LEN;
	ret = (PasswordMessage*)malloc(length + sizeof(PasswordMessage) - sizeof(uint32_t));
	memset(ret, 0, length + sizeof(PasswordMessage) - sizeof(uint32_t));

	ret->type = PSQL_PasswordMessage;
	ret->length = htonl(length);
	memcpy(ret->data, md5_password, MD5_PASSWORD_LEN);
	ret->password = ret->data;

	return ret;
}

static void test_valid_password_message(void **state) {
	PasswordMessage	*password_message;
	char		*user = "user";
	char		*password = "password";
	char		*salt = "salt";

	password_message = make_password_message(user, password, salt);
	assert_non_null(password_message);
	// MD5 of "passworduser" == 4d45974e13472b5a0be3533de4666414
	// MD5 of "4d45974e13472b5a0be3533de4666414salt" == 8e998aaa66bd302e5592df3642c16f78
	assert_string_equal(password_message->password, "md58e998aaa66bd302e5592df3642c16f78");
}

static void test_valid_input(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	// Prepare startup message with username
	char *username = "user";
	StartupMessage *startup_message = make_startup_message("user", username);

	ydb_buffer_t user_info_subs;
	YDB_MALLOC_BUFFER(&user_info_subs, MAX_STR_CONST);
	// md5 hash of passworduser: 4d45974e13472b5a0be3533de4666414
	char *user_info = "1|user|super|inh|crer|cred|canl|repl|bypassrl|conn|md54d45974e13472b5a0be3533de4666414|valid";
	int done = 0;
	YDB_COPY_STRING_TO_BUFFER(user_info, &user_info_subs, done);
	will_return(__wrap_ydb_get_s, &user_info_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, strlen(column_value));

	char *password = "password";
	char *salt = "salt";
	password_message = make_password_message(username, password, salt);

	int32_t result = handle_password_message(password_message, startup_message, salt);
	assert_int_equal(result, 0);

	free(password_message);
	free(startup_message->parameters);
	free(startup_message);
}

static void test_error_not_md5(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char *username = "user";
	StartupMessage *startup_message = make_startup_message("user", username);
	char *password = "password";
	char *salt = "salt";

	// Wrap calls in make_password_message
	will_return(__wrap_octo_log, 0);

	password_message = make_password_message(username, password, salt);
	password_message->password = "password";

	int32_t result = handle_password_message(password_message, startup_message, salt);
	assert_int_equal(result, 1);

	free(password_message);
	free(startup_message->parameters);
	free(startup_message);
}

static void test_error_user_info_lookup(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char *username = "user";
	StartupMessage *startup_message = make_startup_message("user", username);

	ydb_buffer_t user_info_subs;
	YDB_MALLOC_BUFFER(&user_info_subs, MAX_STR_CONST);
	// md5 hash of passworduser: 4d45974e13472b5a0be3533de4666414
	char *user_info = "1|user|super|inh|crer|cred|canl|repl|bypassrl|conn|md54d45974e13472b5a0be3533de4666414|valid";
	int done = 0;
	YDB_COPY_STRING_TO_BUFFER(user_info, &user_info_subs, done);
	will_return(__wrap_ydb_get_s, &user_info_subs);
	will_return(__wrap_ydb_get_s, YDB_ERR_LVUNDEF);

	char *salt = "salt";
	char *password = "password";

	// Wrap calls in make_password_message
	will_return(__wrap_octo_log, 0);

	password_message = make_password_message(username, password, salt);

	int32_t result = handle_password_message(password_message, startup_message, salt);
	assert_int_equal(result, 1);

	free(password_message);
	free(startup_message->parameters);
	free(startup_message);
}

static void test_error_hash_lookup(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char *username = "user";
	StartupMessage *startup_message = make_startup_message("user", username);

	ydb_buffer_t user_info_subs;
	YDB_MALLOC_BUFFER(&user_info_subs, MAX_STR_CONST);
	// md5 hash of passworduser: 4d45974e13472b5a0be3533de4666414
	char *user_info = "1|user|super|inh|crer|cred|canl|repl|bypassrl|conn|md54d45974e13472b5a0be3533de4666414|valid";
	int done = 0;
	YDB_COPY_STRING_TO_BUFFER(user_info, &user_info_subs, done);
	will_return(__wrap_ydb_get_s, &user_info_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, 0);

	// Wrap calls in make_password_message
	will_return(__wrap_octo_log, 0);

	char *salt = "salt";
	char *password = "password";
	password_message = make_password_message(username, password, salt);

	int32_t result = handle_password_message(password_message, startup_message, salt);
	assert_int_equal(result, 1);

	free(password_message);
	free(startup_message->parameters);
	free(startup_message);
}

static void test_error_hash_conversion(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char *username = "user";
	StartupMessage *startup_message = make_startup_message("user", username);

	ydb_buffer_t user_info_subs;
	YDB_MALLOC_BUFFER(&user_info_subs, MAX_STR_CONST);
	// md5 hash of passworduser: 4d45974e13472b5a0be3533de4666414
	char *user_info = "1|user|super|inh|crer|cred|canl|repl|bypassrl|conn|md54d45974e13472b5a0be3533de4666414|valid";
	int done = 0;
	YDB_COPY_STRING_TO_BUFFER(user_info, &user_info_subs, done);
	will_return(__wrap_ydb_get_s, &user_info_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	char *column_value = "md5oopsnotthecorrecthash";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, strlen(column_value));

	char *salt = "salt";
	char *password = "password";
	password_message = make_password_message(username, password, salt);

	// Wrap calls in handle_password_message
	will_return(__wrap_octo_log, 0);

	int32_t result = handle_password_message(password_message, startup_message, salt);
	assert_int_equal(result, 1);

	free(password_message);
	free(startup_message->parameters);
	free(startup_message);
}

static void test_error_bad_password(void **state) {
	PasswordMessage *password_message;
	RoctoSession session;
	ydb_buffer_t session_id;

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	char *username = "user";
	StartupMessage *startup_message = make_startup_message("user", username);

	ydb_buffer_t user_info_subs;
	YDB_MALLOC_BUFFER(&user_info_subs, MAX_STR_CONST);
	// md5 hash of passworduser: 4d45974e13472b5a0be3533de4666414
	char *user_info = "1|user|super|inh|crer|cred|canl|repl|bypassrl|conn|md54d45974e13472b5a0be3533de4666414|valid";
	int done = 0;
	YDB_COPY_STRING_TO_BUFFER(user_info, &user_info_subs, done);
	will_return(__wrap_ydb_get_s, &user_info_subs);
	will_return(__wrap_ydb_get_s, YDB_OK);

	char *column_value = "md54d45974e13472b5a0be3533de4666414";
	will_return(__wrap_get_user_column_value, column_value);
	will_return(__wrap_get_user_column_value, strlen(column_value));

	// Wrap calls in handle_password_message
	will_return(__wrap_octo_log, 0);

	char *salt = "salt";
	char *password = "balugawhales";
	password_message = make_password_message(username, password, salt);

	int32_t result = handle_password_message(password_message, startup_message, salt);
	assert_int_equal(result, 1);

	free(password_message);
	free(startup_message->parameters);
	free(startup_message);
}

static void test_error_missing_username() {
	PasswordMessage	*password_message;
	StartupMessage	*startup_message;
	RoctoSession	session;
	ydb_buffer_t	session_id;
	char *password = "password", *salt = "salt";

	YDB_LITERAL_TO_BUFFER("0", &session_id);
	session.session_id = &session_id;

	startup_message = make_startup_message("database", "dummy_db");

	// Wrap calls in make_password_message
	will_return(__wrap_octo_log, 0);

	password_message = make_password_message("user", password, salt);

	// Since we are missing a `user` field, octo should give an error
	assert_int_not_equal(handle_password_message(password_message, startup_message, salt), 0);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_password_message),
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_error_not_md5),
		   cmocka_unit_test(test_error_user_info_lookup),
		   cmocka_unit_test(test_error_hash_lookup),
		   cmocka_unit_test(test_error_hash_conversion),
		   cmocka_unit_test(test_error_bad_password),
		   cmocka_unit_test(test_error_missing_username),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
