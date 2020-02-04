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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <openssl/md5.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"
#include "helpers.h"

#define	MD5_HEX_LEN	((MD5_DIGEST_LENGTH * 2) + 1)

int handle_password_message(PasswordMessage *password_message, StartupMessage *startup_message, char *salt) {
	char		md5_hex[MD5_HEX_LEN];

	// Check the type of password message, for now just md5 is accepted
	int32_t result = strncmp(password_message->password, "md5", 3);
	if (result != 0) {
		FATAL(ERR_ROCTO_PASSWORD_TYPE, "handle_password_message", "md5");
		return 1;
	}

	// Retrieve username from StartupMessage
	char username[MAX_STR_CONST+1];		// Null terminator
	for(int cur_parm = 0; cur_parm < startup_message->num_parameters; cur_parm++) {
		result = strcmp(startup_message->parameters[cur_parm].name, "user");
		if (0 == result) {
			strncpy(username, startup_message->parameters[cur_parm].value, MAX_STR_CONST);
			break;
		}
	}

	// Retrieve user info from database
	ydb_buffer_t user_info_subs;
	YDB_MALLOC_BUFFER(&user_info_subs, MAX_STR_CONST);
	ydb_buffer_t *user_subs = make_buffers(config->global_names.octo, 2, "users", username);
	result = ydb_get_s(&user_subs[0], 2, &user_subs[1], &user_info_subs);
	free(user_subs);
	if (YDB_ERR_GVUNDEF == result || YDB_ERR_LVUNDEF == result) {
		FATAL(ERR_ROCTO_DB_LOOKUP, "handle_password_message", "user info");
		YDB_FREE_BUFFER(&user_info_subs);
		return 1;
	}
	YDB_ERROR_CHECK(result);
	if (YDB_OK != result) {
		YDB_FREE_BUFFER(&user_info_subs);
		return 1;
	}

	// Extract password hash
	char buffer[MAX_STR_CONST];
	uint32_t buf_len = get_user_column_value(buffer, MAX_STR_CONST, user_info_subs.buf_addr, user_info_subs.len_used,
			UserColumn_ROLPASSWORD);
	YDB_FREE_BUFFER(&user_info_subs);
	if (0 == buf_len) {
		FATAL(ERR_ROCTO_COLUMN_VALUE, "handle_password_message", "rolpassword (hashed password)");
		return 1;
	}

	// Concatenate stored hash with temporary 4-byte salt
	unsigned char hash_buf[MAX_STR_CONST];
	memcpy(hash_buf, &buffer[3], buf_len-3);	// Exclude "md5" from stored password (-3),
	memcpy(&hash_buf[buf_len-3], salt, 4);

	// Hash password hash with temporary 4-byte salt
	MD5(hash_buf, buf_len-3+4, hash_buf);

	// Convert raw md5 hash to hex string
	result = md5_to_hex(hash_buf, md5_hex, MD5_HEX_LEN);
	if (0 != result) {
		FATAL(ERR_ROCTO_HASH_CONVERSION, "handle_password_message", "md5 hash", "hexidecimal string");
		return 1;
	}
	// Compare final hash of stored password against hash sent by client
	result = strncmp(md5_hex, &password_message->password[3], MD5_HEX_LEN);	// Exclude "md5" prefix
	if (0 != result) {
		FATAL(ERR_ROCTO_BAD_PASSWORD, "handle_password_message");
		return 1;
	}
	// Note down that user authenticated successfully without notifying the client,
	// as the client doesn't expect any notifications during authentication
	/*rocto_session.sending_message = TRUE;
	INFO(INFO_AUTH_SUCCESS, "handle_password_message");
	rocto_session.sending_message = FALSE;*/
	LOG_LOCAL_ONLY(INFO, INFO_AUTH_SUCCESS, "handle_password_message");

	return 0;
}
