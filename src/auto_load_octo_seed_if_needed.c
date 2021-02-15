/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "git_hashes.h"

#define CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB)      \
	{                                                                       \
		if (RELEASE_DDL_LOCK) {                                         \
			int lclStatus;                                          \
                                                                                \
			lclStatus = ydb_lock_decr_s(&OCTO_GLOBAL, 1, &LOCKSUB); \
			YDB_ERROR_CHECK(lclStatus);                             \
		}                                                               \
		return STATUS;                                                  \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB)    \
	{                                                                                   \
		if (YDB_OK != STATUS) {                                                     \
			YDB_ERROR_CHECK(STATUS);                                            \
			CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB); \
		}                                                                           \
	}

/* Checks if the octo seed file needs to be reloaded due to a new Octo build opening this database for the first time.
 * If so, it auto loads the octo-seed.sql and octo-seed.zwr files.
 * Returns YDB_OK on success and a non-zero (YDB_ERR_* status code or 1) on errors.
 * Note: This code is similar to "src/auto_upgrade_binary_definition_if_needed.c".
 */
int auto_load_octo_seed_if_needed(void) {
	ydb_buffer_t octo_global, subs, fmt;
	char	     fmt_buff[sizeof(YDBOCTO_GIT_COMMIT_VERSION)];
	int	     status;
	boolean_t    auto_load_needed, release_ddl_lock;
	ydb_buffer_t locksub;
	boolean_t    save_allow_schema_changes;

	/* Check if binary definitions (tables or functions) need to be auto upgraded. They need to be if
	 * ^%ydboctoocto(OCTOLIT_SEEDFMT) is different from YDBOCTO_GIT_COMMIT_VERSION.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(OCTOLIT_SEEDFMT, &subs);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_DDL, &locksub);
	fmt.buf_addr = &fmt_buff[0];
	fmt.len_alloc = sizeof(fmt_buff);
	for (release_ddl_lock = FALSE;; release_ddl_lock = TRUE) {
		status = ydb_get_s(&octo_global, 1, &subs, &fmt);
		switch (status) {
		case YDB_ERR_GVUNDEF:
			auto_load_needed = TRUE;
			break;
		case YDB_OK:
			assert(fmt.len_used < sizeof(fmt_buff));
			fmt.buf_addr[fmt.len_used] = '\0';
			auto_load_needed = memcmp(fmt.buf_addr, YDBOCTO_GIT_COMMIT_VERSION, fmt.len_used + 1);
			break;
		default:
			YDB_ERROR_CHECK(status);
			CLEANUP_AND_RETURN(status, release_ddl_lock, octo_global, locksub);
			assert(FALSE);
		}
		if (!auto_load_needed) {
			/* No auto upgrade needed. Return after releasing ddl exclusive lock (if applicable). */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
			return YDB_OK;
		}
		if (!release_ddl_lock) {
			/* Get exclusive lock (same as the one used in "run_query.c" to modify DDL) */
			status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &octo_global, 1, &locksub);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
			/* Now that we have the exclusive lock, check again if the octo seed format is still different
			 * (could have been concurrently fixed). Hence we go one more iteration in this for loop.
			 */
		} else {
			break;
		}
	}
	/* In case this is a rocto process, it is not allowed to do schema changes by default. But allow the auto upgrade
	 * for this process. Hence the temporary modification to "config->allow_schema_changes" below.
	 */
	save_allow_schema_changes = config->allow_schema_changes;
	config->allow_schema_changes = TRUE;
	/* Set a global variable to indicate this is the small window where auto load of octo-seed.sql runs. This lets
	 * "run_query.c" know to do some special processing (e.g. run DROP TABLE before a CREATE TABLE etc.).
	 */
	assert(FALSE == config->in_auto_load_octo_seed);
	config->in_auto_load_octo_seed = TRUE;
	status = auto_load_octo_seed();
	config->in_auto_load_octo_seed = FALSE;
	config->allow_schema_changes = save_allow_schema_changes;
	if (0 != status) {
		CLEANUP_AND_RETURN(status, release_ddl_lock, octo_global, locksub);
	}
	/* Now that auto load is complete, indicate that (so other processes do not attempt the auto load)
	 * by setting ^%ydboctoocto(OCTOLIT_SEEDFMT) to YDBOCTO_GIT_COMMIT_VERSION.
	 */
	fmt.len_used = snprintf(fmt.buf_addr, fmt.len_alloc, "%s", YDBOCTO_GIT_COMMIT_VERSION);
	status = ydb_set_s(&octo_global, 1, &subs, &fmt);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);

	/* Now that auto load is complete, release exclusive lock */
	status = ydb_lock_decr_s(&octo_global, 1, &locksub);
	release_ddl_lock = FALSE; /* needed so the below macro invocation does not try to release the lock again */
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);

	return YDB_OK;
}
