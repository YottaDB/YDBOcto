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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <openssl/md5.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

int handle_password_message(PasswordMessage *password_message, RoctoSession *session, ErrorResponse **err) {
	ErrorBuffer err_buff;
	const char *error_message;
	const unsigned int md5_hex_len = MD5_DIGEST_LENGTH * 2 + 1;
	// Check the type of password message, for now just md5 is accepted
	int result = strncmp(password_message->password, "md5", 3);
	if (result != 0) {
		WARNING(ERR_ROCTO_PASSWORD_TYPE, "handle_password_message", "md5");
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "PasswordMessage");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		return 1;
	}

	// Retrieve username from session info
	ydb_buffer_t username_subs;
	result = ydb_get_s(config->global_names.session, 3, session->session_id->buf_addr, "variables", &username_subs);
	if (YDB_OK != result) {
		WARNING(CUSTOM_ERROR, "handle_password_message: failed to retrieve username from session info");
		error_message = format_error_string(&err_buff, CUSTOM_ERROR,
				"handle_password_message: failed to retrieve username from session info");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		return 1;
	}
	char *username = username_subs.buf_addr;
	username[username_subs.len_used]  = '\0';

	// Retrieve user info from database
	ydb_buffer_t user_info_subs;
	ydb_buffer_t *user_subs = makebuffers(config->global_names.octo, 2, "users", username);
	result = ydb_get_s(&user_subs[0], 2, &user_subs[1], &user_info_subs);
	if (YDB_OK != result) {
		WARNING(CUSTOM_ERROR, "handle_password_message: failed to retrieve user info from database");
		error_message = format_error_string(&err_buff, CUSTOM_ERROR,
				"handle_password_message: failed to retrieve user info from database");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(user_subs);
		return 1;
	}
	char *user_info = user_info_subs.buf_addr;
	user_info[user_info_subs.len_used]  = '\0';

	// Extract password hash
	char buffer[MAX_STR_CONST];
	unsigned int buf_len = get_user_column_value(buffer, MAX_STR_CONST, user_info, user_info_subs->len_used, ROLPASSWORD);
	if (0 == buf_len) {
		WARNING(CUSTOM_ERROR, "handle_password_message: failed to retrieve user password hash");
		error_message = format_error_string(&err_buff, CUSTOM_ERROR,
				"handle_password_message: failed to retrieve user password hash");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(user_subs);
		return 1;
	}

	// Retrieve temporary salt from session info
	ydb_buffer_t salt_subs;
	ydb_buffer_t *session_salt_subs = makebuffers(config->global_names.session, 3, session->session_id->buf_addr, "auth", "salt");
	result = ydb_get_s(&session_salt_subs[0], 3, &session_salt_subs[1], &salt_subs);
	if (YDB_OK != result) {
		WARNING(CUSTOM_ERROR, "handle_password_message: failed to retrieve temporary salt from session info");
		error_message = format_error_string(&err_buff, CUSTOM_ERROR,
				"handle_password_message: failed to retrieve temporary salt from session info");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(user_subs);
		free(session_salt_subs);
		return 1;
	}
	char *salt = salt_subs.buf_addr;
	salt[salt_subs.len_used]  = '\0';

	// Concatenate stored hash with temporary 4-byte salt
	char hash_buf[MAX_STR_CONST];
	snprintf(hash_buf, buf_len + 4 - 3, "%s%s", &buffer[3], salt);	// Exclude "md5" prefix

	// Hash password hash with temporary 4-byte salt
	MD5(hash_buf, strlen(hash_buf), hash_buf);

	// Convert raw md5 hash to hex string
	char md5_hex[md5_hex_len];
	result = md5_to_hex(hash_buf, md5_hex, md5_hex_len);
	if (0 != result) {
		WARNING(CUSTOM_ERROR, "handle_password_message: failed to convert md5 hash to hexidecimal string");
		error_message = format_error_string(&err_buff, CUSTOM_ERROR,
				"handle_password_message: failed to convert md5 hash to hexidecimal string");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(user_subs);
		free(session_salt_subs);
		return 1;
	}

	// Compare final hash of stored password against hash sent by client
	result = strncmp(md5_hex, &password_message->password[3], md5_hex_len);	// Exclude "md5" prefix
	if (0 != result) {
		WARNING(ERR_ROCTO_BAD_PASSWORD, "handle_password_message");
		error_message = format_error_string(&err_buff, ERR_ROCTO_BAD_PASSWORD, "handle_password_message");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		free(user_subs);
		free(session_salt_subs);
		return 1;
	}
	INFO(CUSTOM_ERROR, "handle_password_message: user successfully authenticated");

	free(user_subs);
	free(session_salt_subs);

	return 0;
}
