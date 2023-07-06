/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "helpers.h"
#include "template_helpers.h"
#include "pg_defaults.h"

/* Updates the value of the specified runtime variable from pg_settings.
 *
 * Return values:
 *	On success: There are two possible non-error cases:
 *		1. Returns YDB_OK for normal exit
 *		2. Returns 2 if variable is either "user" or "database", which are not actual runtime parameters, but are sent as if
 *		   they were by remote PostgreSQL clients. Accordingly, this return value only occurs in rocto.
 *	On error:
 *		Returns 1
 */
int set_parameter_in_pg_settings(char *variable, char *value) {
	ydb_buffer_t pg_buffers[5];
	ydb_buffer_t value_buffer;
	ydb_string_t ci_variable, ci_value;
	char *	     column_end, *new_row;
	int	     status;
	int	     value_len, row_len, copied;

	// SET a runtime variable to a specified value by updating the appropriate session LVN
	assert((NULL != variable) && (NULL != value));
	// Lowercase the variable name to enforce case insensitivity
	TOLOWER_STR(variable);

	/* Initialize session LVN subscripts
	 *
	 * Use global_names.raw_octo to access table LVN, i.e. "%ydboctoocto" instead of "^%ydboctoocto":
	 *	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_PG_SETTINGS,runtime_variable_stmt)
	 *
	 * Note that the runtime_variable_stmt will be lower case due to lexer conversion of identifiers.
	 * Accordingly, first retrieve the canonical form of the variable name (typically lowercase, per PostgreSQL convention)
	 * from the lower-to-canonical name mapping done at startup in octo_init.c by load_pg_defaults().
	 */
	YDB_STRING_TO_BUFFER(config->global_names.raw_octo, &pg_buffers[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_SETTINGS, &pg_buffers[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PG_SETTINGS, &pg_buffers[2]);
	YDB_STRING_TO_BUFFER(variable, &pg_buffers[3]);
	/* Prepare the variable name as a ydb_ci parameter here to reuse length determination done by YDB_STRING_TO_BUFFER before it
	 * is overwritten by ydb_get_s below, saving a strlen call.
	 */
	ci_variable.address = variable;
	ci_variable.length = pg_buffers[3].len_used;

	/* Lookup the row in `pg_settings` corresponding to the specified runtime parameter:
	 *	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_PG_SETTINGS,canonical_name)
	 */
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&value_buffer, OCTO_INIT_BUFFER_LEN);
	YDB_STRING_TO_BUFFER(OCTOLIT_PG_SETTINGS, &pg_buffers[2]);
	status = ydb_get_s(&pg_buffers[0], 3, &pg_buffers[1], &value_buffer);
	if (YDB_ERR_INVSTRLEN == status) {
		EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
		status = ydb_get_s(&pg_buffers[0], 3, &pg_buffers[1], &value_buffer);
		assert(YDB_ERR_INVSTRLEN != status);
	}
	if (YDB_ERR_LVUNDEF == status) {
		/* Note: name is null-terminated during read_startup_message(), while the macros are compile-time
		 * string literals and thus null-terminated then.
		 */
		YDB_FREE_BUFFER(&value_buffer);
		if ((0 == strcmp(variable, OCTOLIT_USER_LOWER)) || (0 == strcmp(variable, OCTOLIT_DATABASE_LOWER))) {
			/* StartupMessages will contain "user" and "database" fields that are not in fact
			 * run-time parameters, but variables used for authentication only. Accordingly, they should
			 * not be stored in the database. So signal to the caller to skip these fields in this case by returning `2`
			 * and omit the usual error message and return value of `1`.
			 *
			 * Note that this manual check is necessary since StartupMessage parameters may appear in an
			 * arbitrary order.
			 *
			 * These are the only known exceptional cases found in actual StartupMessages received from
			 * the client. If others are discovered, those would also need to be accounted for in a
			 * similar fashion.
			 */
			return 2;
		}
		if ((0 == strcmp(variable, OCTOLIT_IS_SUPERUSER_LOWER))
		    || (0 == strcmp(variable, OCTOLIT_SESSION_AUTHORIZATION_LOWER))) {
			ERROR(ERR_PARM_CANNOT_BE_CHANGED, variable);
		} else {
			// The specified runtime variable is invalid, i.e. doesn't exist. Issue error.
			ERROR(ERR_INVALID_RUNTIME_PARAMETER, variable);
		}
		return 1;
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&value_buffer);
		return 1;
	}
	value_buffer.buf_addr[value_buffer.len_used] = '\0';

	// Find new row length and allocate space for the new string
	value_len = strlen(value);					 // Get length of new value string
	column_end = strchr(value_buffer.buf_addr, (*COLUMN_DELIMITER)); // Find first column delimiter
	row_len = (value_buffer.buf_addr + value_buffer.len_used) - column_end;
	row_len += value_len + sizeof(char); // Length of the new value + null terminator
	new_row = (char *)malloc(sizeof(char) * row_len);
	// Update the value of the parameter, i.e. the first column value
	copied = snprintf(new_row, row_len, "%s%s", value, column_end);
	UNUSED(copied);
	assert(copied < row_len); // Confirm no truncation
	YDB_FREE_BUFFER(&value_buffer);

	ci_value.address = value;
	ci_value.length = value_len;
	status = ydb_ci("_ydboctoxrefupdate", &ci_variable, &ci_value);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		free(new_row);
		return 1;
	}

	// Replace the previous row data for the specified parameter with the new row data (which has the updated value)
	YDB_STRING_TO_BUFFER(new_row, &value_buffer);
	status = ydb_set_s(&pg_buffers[0], 3, &pg_buffers[1], &value_buffer);
	free(new_row);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}

	return YDB_OK;
}
