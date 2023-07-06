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

#include "octo.h"
#include "pg_defaults.h"
#include "assert.h"

#define LOAD_PG_VARIABLE(NAME, ROW_DEFAULT)                                                                        \
	{                                                                                                          \
		int	     status;                                                                               \
		ydb_buffer_t pg_buffers[4];                                                                        \
		ydb_buffer_t value_buffer;                                                                         \
                                                                                                                   \
		/* Load the default row data for the given runtime parameter into pg_settings for later access. */ \
		/*	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_PG_SETTINGS,NAME)=ROW_DEFAULT */                          \
		YDB_STRING_TO_BUFFER(config->global_names.raw_octo, &pg_buffers[0]);                               \
		YDB_STRING_TO_BUFFER(OCTOLIT_SETTINGS, &pg_buffers[1]);                                            \
		YDB_STRING_TO_BUFFER(OCTOLIT_PG_SETTINGS, &pg_buffers[2]);                                         \
		YDB_STRING_TO_BUFFER(NAME, &pg_buffers[3]);                                                        \
		YDB_STRING_TO_BUFFER(ROW_DEFAULT, &value_buffer);                                                  \
                                                                                                                   \
		status = ydb_set_s(&pg_buffers[0], 3, &pg_buffers[1], &value_buffer);                              \
		/* LOAD_PG_VARIABLE is to be run at process startup for proper support of runtime variables, */    \
		/* and so should not fail. */                                                                      \
		assert(YDB_OK == status);                                                                          \
		YDB_ERROR_CHECK(status);                                                                           \
		if (YDB_OK != status) {                                                                            \
			return status;                                                                             \
		}                                                                                                  \
	}

#define LOAD_READ_ONLY_VARIABLE(NAME, DEFAULT_VALUE)                                                                   \
	{                                                                                                              \
		int	     status;                                                                                   \
		ydb_buffer_t pg_buffers[5];                                                                            \
		ydb_buffer_t value_buffer;                                                                             \
                                                                                                                       \
		/* Construct the subscript array for storing the read-only run-time parameter value */                 \
		/* This value will not be stored in `pg_settings` and will only be viewable via SHOW commands. */      \
		/*	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_READ_ONLY,NAME)=DEFAULT_VALUE */                              \
		YDB_STRING_TO_BUFFER(config->global_names.raw_octo, &pg_buffers[0]);                                   \
		YDB_STRING_TO_BUFFER(OCTOLIT_SETTINGS, &pg_buffers[1]);                                                \
		YDB_STRING_TO_BUFFER(OCTOLIT_READ_ONLY, &pg_buffers[2]);                                               \
		YDB_STRING_TO_BUFFER(NAME, &pg_buffers[3]);                                                            \
		YDB_STRING_TO_BUFFER(DEFAULT_VALUE, &value_buffer);                                                    \
                                                                                                                       \
		status = ydb_set_s(&pg_buffers[0], 3, &pg_buffers[1], &value_buffer);                                  \
		/* LOAD_READ_ONLY_VARIABLE is to be run at process startup for proper support of runtime variables, */ \
		/* and so should not fail. */                                                                          \
		assert(YDB_OK == status);                                                                              \
		YDB_ERROR_CHECK(status);                                                                               \
		if (YDB_OK != status) {                                                                                \
			return status;                                                                                 \
		}                                                                                                      \
	}

int load_pg_defaults(void) {
#include "pg_defaults_table.h" /* this would return with a value that is not YDB_OK in case of errors */
	return YDB_OK;
}
