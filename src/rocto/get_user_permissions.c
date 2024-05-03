/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
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
#include "rocto.h"
#include "helpers.h"

int get_user_permissions(RoctoSession *session) {
	int	     status;
	long	     temp_long;
	ydb_buffer_t session_subs[2], user_subs[4];
	ydb_buffer_t permissions_buf;
	char	     username[OCTO_MAX_IDENT + 1];	   // Null terminator
	char	     permissions_str[INT32_TO_STRING_MAX]; // Includes null terminator

	YDB_STRING_TO_BUFFER(config->global_names.session, &session_subs[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_USER, &session_subs[1]);

	YDB_STRING_TO_BUFFER(config->global_names.octo, &user_subs[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_USERS, &user_subs[1]);
	OCTO_SET_NULL_TERMINATED_BUFFER(user_subs[2], username);
	/* Lookup current user name for use in later user permissions lookup at
	 *	%ydboctosession("user")
	 */
	status = ydb_get_s(&session_subs[0], 1, &session_subs[1], &user_subs[2]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	user_subs[2].buf_addr[user_subs[2].len_used] = '\0';
	YDB_STRING_TO_BUFFER(OCTOLIT_PERMISSIONS, &user_subs[3]);

	/* Get the user permissions information, stored as an integer at
	 *	^%ydboctoocto("users",username,"permissions")
	 */
	OCTO_SET_NULL_TERMINATED_BUFFER(permissions_buf, permissions_str);
	status = ydb_get_s(&user_subs[0], 3, &user_subs[1], &permissions_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	permissions_buf.buf_addr[permissions_buf.len_used] = '\0';

	// Convert user permissions integer string to int32_t and store in session->permissions
	temp_long = strtol(permissions_buf.buf_addr, NULL, 10);
	if (!STRTOL_VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (INT32_MAX >= temp_long)) {
		session->permissions = (int32_t)temp_long;
	} else {
		FATAL(ERR_ROCTO_PERMISSIONS_LOOKUP_FAILED, username);
		return 1;
	}

	// Add username to session struct for later use in error messages
	memcpy(session->username, user_subs[2].buf_addr, user_subs[2].len_used);
	session->username[user_subs[2].len_used] = '\0';

	return 0;
}
