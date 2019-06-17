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

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

int handle_password_message(PasswordMessage *password_message, RoctoSession *session, ErrorResponse **err) {
	int result = 0;
	size_t password_length = 0;
	char *username = NULL, *password = NULL, *salt = NULL;
	ydb_buffer_t *username_subs = NULL, *password_subs = NULL, *salt_subs = NULL, *session_salt_subs = NULL;
	password_length  = password_message->length - sizeof(int);	// exclude length

	// Check the type of password message, for now just md5 is accepted
	result = strncmp(password_message->password, "md5", 3);
	if (result != 0) {
		WARNING(ERR_ROCTO_PASSWORD_TYPE, "handle_password_message", "md5");
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "PasswordMessage");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Syntax_Error,
					   error_message,
					   0);
		return 1
	}

	// Retrieve username from session info
	result = ydb_get_s(config->global_names.session, 3, rocto_session.session_id->buf_addr, "variables", username_subs);
	if (YDB_OK != result) {
		// error
		return 1
	}
	username = username_subs->buf_addr;
	username[username_subs->buf_len]  = '\0';

	// Retrieve user info from database
	user_subs = makebuffers(config->global_names.octo, 2, "users", username);
	result = ydb_get_s(&user_subs[0], 2, &user_subs[1], user_info_subs);
	user_info = user_info_subs.buf_addr;
	user_info[user_info_subs->buf_len]  = '\0';

	// Extract password hash

	// Retrieve temporary salt from session info
	// session_salt_subs = makebuffers(
	result = ydb_get_s(config->global_names.session, 3, rocto_session.session_id->buf_addr, "auth", "salt", salt_subs);
	salt = salt_subs->buf_addr;
	salt[salt_subs->buf_len]  = '\0';


	return 0;
}
