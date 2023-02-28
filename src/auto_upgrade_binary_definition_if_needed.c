/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <assert.h>

#include "octo.h"

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

/* Checks if the binary table/function definitions need to be auto upgraded. If so, it auto upgrades them.
 * Returns YDB_OK on success and a non-zero (YDB_ERR_* status code or 1) on errors.
 * Note: This code is similar to "src/auto_upgrade_plan_definition_if_needed.c".
 */
int auto_upgrade_binary_definition_if_needed(void) {
	ydb_buffer_t octo_global, subs, fmt;
	char	     fmt_buff[INT32_TO_STRING_MAX];
	int	     status;
#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
	int binary_definition_fmt;
#endif
	boolean_t    auto_upgrade_needed, release_ddl_lock;
	ydb_buffer_t locksub;

#ifdef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
	if (NULL != getenv("disable_auto_upgrade")) {
		/* Caller set an env var to indicate auto upgrade has to be disabled (e.g. TMU02 bats subtest). Skip it. */
		return YDB_OK;
	}
#endif
	/* Check if binary definitions (tables or functions) need to be auto upgraded. They need to be if
	 * ^%ydboctoocto(OCTOLIT_BINFMT) is different from FMT_BINARY_DEFINITION.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(OCTOLIT_BINFMT, &subs);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_DDL, &locksub);
	fmt.buf_addr = &fmt_buff[0];
	fmt.len_alloc = sizeof(fmt_buff);
	for (release_ddl_lock = FALSE;; release_ddl_lock = TRUE) {
		status = ydb_get_s(&octo_global, 1, &subs, &fmt);
		switch (status) {
		case YDB_ERR_GVUNDEF:
			auto_upgrade_needed = TRUE;
			break;
		case YDB_OK:
			assert(fmt.len_used < sizeof(fmt_buff));
			fmt.buf_addr[fmt.len_used] = '\0';
#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
			binary_definition_fmt = atoi(fmt.buf_addr);
			auto_upgrade_needed = (FMT_BINARY_DEFINITION != binary_definition_fmt);
#else
			auto_upgrade_needed = TRUE;
#endif
			break;
		default:
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
			break;
		}
		if (!auto_upgrade_needed) {
			/* No auto upgrade needed. Return after releasing ddl exclusive lock (if applicable). */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
			return YDB_OK;
		}
		if (!release_ddl_lock) {
			/* Get exclusive lock (same as the one used in "run_query.c" to modify DDL) */
			status = ydb_lock_incr_s(TIMEOUT_DDL_EXCLUSIVELOCK, &octo_global, 1, &locksub);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
			/* Now that we have the exclusive lock, check again if the binary table definition is still different
			 * (could have been concurrently fixed). Hence we go one more iteration in this for loop.
			 */
		} else {
			break;
		}
	}
	UPGRADE_BINARY_DEFINITIONS_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
	assert(YDB_OK == status); // Ensure above call succeeded
	/* Now that auto upgrade is complete, release exclusive lock */
	status = ydb_lock_decr_s(&octo_global, 1, &locksub);
	release_ddl_lock = FALSE; /* needed so the below macro invocation does not try to release the lock again */
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);

	return YDB_OK;
}
