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
	ydb_buffer_t *user_subs = NULL, *user_id_subs = NULL, *rolname_subs = NULL, *password_subs = NULL;
	ydb_buffer_t *username_subs = NULL;
	char *username = NULL;
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
	user_subs = make_buffers(config->global_names.octo, 1, "user");
	YDB_STRING_TO_BUFFER("rolname", rolname_subs);
	unsigned int i = 1;
	// char user_id[MAX_STR_CONST];
	while (YDB_ERR_NODEEND != result) {
		// snprintf(user_id, MAX_STR_CONST, "%u", i);
		// YDB_STRING_TO_BUFFER(user_id, user_id_sub);
		result = ydb_subscript_next_s(&user_subs[0], 1, &user_subs[1], user_id_subs);
		username_subs = make_buffers(&user_subs[0], 3, "user", user_id_subs->buf_addr, "rolname");
		result = ydb_get_s(&username_subs[0], 3, &username_subs[1], password_subs);
		if (0 == strcmp
	}
	result = ydb_get_s(
	return 0;
}
