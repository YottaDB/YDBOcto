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

#include <assert.h>
#include <string.h>

#include "octo.h"

#define EXTRACT_VALUE(BUFFER, STR, LEN)                                                                      \
	{                                                                                                    \
		char *column_end;                                                                            \
                                                                                                             \
		/* Extract the value to be SHOWn */                                                          \
		column_end = strchr(BUFFER.buf_addr, (*COLUMN_DELIMITER)); /* Find first column delimiter */ \
		*LEN = column_end - BUFFER.buf_addr;                                                         \
		STR = (char *)malloc(sizeof(char) * (*LEN + sizeof(char))); /* Null terminator */            \
		memcpy(STR, BUFFER.buf_addr, *LEN);                                                          \
		(STR)[*LEN] = '\0';                                                                          \
	}

#define POPULATE_OUT_BUFFER_IF_NOT_NULL(OUT, STR, LEN)                                                      \
	{                                                                                                   \
		/* Populate a ydb_buffer_t for use by the caller to reuse the length of the value string */ \
		/* rather than recalculating it when value_len goes out of scope. */                        \
		if (NULL != OUT) {                                                                          \
			(OUT)->buf_addr = STR; /* Must be freed by caller */                                \
			(OUT)->len_used = LEN;                                                              \
			(OUT)->len_alloc = LEN + sizeof(char); /* Null terminator */                        \
		}                                                                                           \
	}

/* Looks up the value of a runtime variable from pg_settings, stores it in a string, and returns this to the caller for handling.
 *
 * Return values:
 *	On success: A string is returned. This string is malloc'd here and so must be freed by the caller. Note that in this case
 *		out->buf_addr is also initialized to the same pointer.
 *	On error: A NULL value is returned.
 */
char *get_parameter_from_pg_settings(char **variable, ydb_buffer_t *out) {
	ydb_buffer_t pg_buffers[4];
	ydb_buffer_t value_buffer;
	char	    *value_str;
	int	     status;
	int	     value_len;
	unsigned int data_ret;
	boolean_t    check_ro = FALSE; // For tracking whether a read-only variable check is necessary

	assert(NULL != variable);
	// Lowercase the variable name to enforce case insensitivity
	TOLOWER_STR(*variable);
	/* Initialize session LVN subscripts
	 *
	 * Use global_names.raw_octo to access table LVN, i.e. "%ydboctoocto" instead of "^%ydboctoocto":
	 *	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_PG_SETTINGS,variable)
	 *
	 * Note that the variable will be upper case due to lexer conversion of identifiers.
	 * Accordingly, first retrieve the canonical form of the variable name (typically lowercase, per PostgreSQL convention)
	 * from the upper-to-canonical name mapping done at startup in octo_init.c by load_pg_defaults().
	 */
	YDB_STRING_TO_BUFFER(config->global_names.raw_octo, &pg_buffers[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_SETTINGS, &pg_buffers[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PG_SETTINGS, &pg_buffers[2]);
	YDB_STRING_TO_BUFFER(*variable, &pg_buffers[3]);

	/* Lookup the canonical name of the parameter and use it to replace the upper case form.
	 * This canonical form will be used to lookup the correct row in `pg_settings` below.
	 *
	 * Note that this will overwrite the pre-allocated space in variable.
	 */
	status = ydb_data_s(&pg_buffers[0], 3, &pg_buffers[1], &data_ret);
	if (0 == data_ret) {
		/* The variable may be either invalid or read-only (and so stored elsewhere).
		 * So, check whether a read-only variable is requested, otherwise issue an error for an invalid one.
		 */
		YDB_STRING_TO_BUFFER(OCTOLIT_READ_ONLY, &pg_buffers[2]);
		check_ro = TRUE;
		status = YDB_OK;
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return NULL;
	}

	/* Lookup the row in `pg_settings` corresponding to the specified runtime parameter:
	 *	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_PG_SETTINGS,canonical_name)
	 */
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&value_buffer, OCTO_INIT_BUFFER_LEN);
	if (!check_ro) {
		YDB_STRING_TO_BUFFER(OCTOLIT_PG_SETTINGS, &pg_buffers[2]);
	}
	status = ydb_get_s(&pg_buffers[0], 3, &pg_buffers[1], &value_buffer);
	if (YDB_ERR_INVSTRLEN == status) {
		EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
		status = ydb_get_s(&pg_buffers[0], 3, &pg_buffers[1], &value_buffer);
		assert(YDB_ERR_INVSTRLEN != status);
	}
	if (YDB_OK == status) {
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		if (check_ro) {
			POPULATE_OUT_BUFFER_IF_NOT_NULL(out, value_buffer.buf_addr, value_buffer.len_used);
			/* All read-only variable names are canonically lowercase, so convert here for consistency when reporting to
			 * clients.
			 */
			pg_buffers[3].buf_addr[pg_buffers[3].len_used] = '\0';
			TOLOWER_STR(pg_buffers[3].buf_addr);
			/* Reusing buffer allocated by OCTO_MALLOC_NULL_TERMINATED_BUFFER above, so return here and skip the
			 * YDB_FREE_BUFFER call below.
			 */
			value_str = value_buffer.buf_addr;
			return value_str;
		} else {
			EXTRACT_VALUE(value_buffer, value_str, &value_len); // Initializes/allocates value_str
			POPULATE_OUT_BUFFER_IF_NOT_NULL(out, value_str, value_len);
		}
	} else {
		if (YDB_ERR_LVUNDEF == status) {
			// The specified runtime variable is invalid, i.e. doesn't exist. Issue error.
			ERROR(ERR_INVALID_RUNTIME_PARAMETER, *variable);
		} else {
			YDB_ERROR_CHECK(status);
		}
		value_str = NULL;
	}
	YDB_FREE_BUFFER(&value_buffer);

	return value_str;
}
