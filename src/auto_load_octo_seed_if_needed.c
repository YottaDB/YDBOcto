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

#include <assert.h>

#include "octo.h"
#include "git_hashes.h"

#define OCTO_LIT_SEEDFMT "seedfmt"

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

/* Previous to YDBOcto#940, ^%ydboctoocto("seedfmt") was used to hold git SHA
 * and if this value was different than the current git SHA octo-seed was reloaded.
 * After YDBOcto#940, ^%ydboctoocto("seeddfnfmt") is used to hold `FMT_SEED_DEFINITION`
 * value and if the stored value is different from current `FMT_SEED_DEFINITION` then octo-seed is reloaded.
 * Since there is a change in the way octo-seed reload was tracked, following macro is used to remove
 * older gvn(^%ydboctoocto("seedfmt") which tracked octo-seed reload if it exists.
 */
#define DELETE_SEED_FMT_GVN()                                                               \
	{                                                                                   \
		ydb_buffer_t seed_fmt_buf, lcl_octo_global;                                 \
		YDB_STRING_TO_BUFFER(OCTO_LIT_SEEDFMT, &seed_fmt_buf);                      \
		int dlqStatus;                                                              \
                                                                                            \
		YDB_STRING_TO_BUFFER(config->global_names.octo, &lcl_octo_global);          \
		dlqStatus = ydb_delete_s(&lcl_octo_global, 1, &seed_fmt_buf, YDB_DEL_TREE); \
		YDB_ERROR_CHECK(dlqStatus);                                                 \
	}

/* Checks if the octo seed file needs to be reloaded due to a new Octo build opening this database for the first time.
 * If so, it auto loads the octo-seed.sql and octo-seed.zwr files.
 * Returns YDB_OK on success and a non-zero (YDB_ERR_* status code or 1) on errors.
 * Note: This code is similar to "src/auto_upgrade_binary_definition_if_needed.c".
 */
int auto_load_octo_seed_if_needed(void) {
	ydb_buffer_t octo_global, subs, fmt;
	char	     fmt_buff[INT32_TO_STRING_MAX];
	int	     status;
	boolean_t    full_auto_load_needed, partial_auto_load_needed, release_ddl_lock;
	ydb_buffer_t locksub;
	boolean_t    save_allow_schema_changes;

	full_auto_load_needed = FALSE;
	partial_auto_load_needed = FALSE;
	/* Check if binary definitions (tables or functions) need to be auto upgraded. They need to be if
	 * ^%ydboctoocto(OCTOLIT_SEEDDFNFMT) is different from FMT_SEED_DEFINITION.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(OCTOLIT_SEEDDFNFMT, &subs);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_DDL, &locksub);
	fmt.buf_addr = &fmt_buff[0];
	fmt.len_alloc = sizeof(fmt_buff);
	for (release_ddl_lock = FALSE;; release_ddl_lock = TRUE) {
		status = ydb_get_s(&octo_global, 1, &subs, &fmt);
		switch (status) {
		case YDB_ERR_GVUNDEF:;
			ydb_buffer_t lcl_sub, lcl_fmt;
			char	     lcl_fmt_buff[INT32_TO_STRING_MAX];

			lcl_fmt.buf_addr = &lcl_fmt_buff[0];
			lcl_fmt.len_alloc = sizeof(lcl_fmt_buff);
			YDB_STRING_TO_BUFFER(OCTOLIT_BINFMT, &lcl_sub);
			status = ydb_get_s(&octo_global, 1, &lcl_sub, &lcl_fmt);
			switch (status) {
			case YDB_OK:
				/* OCTOLIT_SEEDDFNFMT gvn is not found but OCTOLIT_BINFMT gvn is found.
				 * This means the current environment was in use by an Octo build (prior to 0939090a
				 * which is when OCTOLIT_SEEDDFNFMT gvn started getting set) and user tables/functions
				 * could have been defined. So
				 * 1) Need to load "octo-seed.sql".
				 * 2) In addition we also need to upgrade the binary definitions of user-defined
				 *    tables/functions since those could have dependencies on functions in "octo-seed.sql"
				 *    (whose binary definitions will change due to loading "octo-seed.sql" in Step 1).
				 * Hence need a FULL auto load.
				 */
				full_auto_load_needed = TRUE;
				DELETE_SEED_FMT_GVN();
				break;
			case YDB_ERR_GVUNDEF:
				/* OCTOLIT_SEEDDFNFMT gvn is not found AND OCTOLIT_BINFMT gvn is also not found.
				 * Note we check OCTOLIT_BINFMT instead of OCTOLIT_SEEDFMT here since the former
				 * pre-dates the latter. OCTOLIT_BINFMT gvn was defined in the earliest commit that
				 * started supporting auto upgrade. So if that gvn is not to be seen, it is safe to
				 * assume no auto upgrade of the binary table/function definitions is needed.
				 * But since OCTOLIT_SEEDDFNFMT gvn is not found either, we need to load "octo-seed.sql".
				 * Hence the need for a PARTIAL auto load.
				 */
				partial_auto_load_needed = TRUE;
				break;
			default:
				YDB_ERROR_CHECK(status);
				CLEANUP_AND_RETURN(status, release_ddl_lock, octo_global, locksub);
				assert(FALSE);
				break;
			}
#ifndef NDEBUG
			// Force auto load if config->seedreload is TRUE
			full_auto_load_needed = (config->seedreload) ? TRUE : full_auto_load_needed;
#endif
			break;
		case YDB_OK:;
			assert(fmt.len_used < sizeof(fmt_buff));
			fmt.buf_addr[fmt.len_used] = '\0';
			int seed_definition_fmt = atoi(fmt.buf_addr);
			full_auto_load_needed = (FMT_SEED_DEFINITION != seed_definition_fmt);
#ifndef NDEBUG
			// Force auto load if config->seedreload is TRUE
			full_auto_load_needed = (config->seedreload) ? TRUE : full_auto_load_needed;
#endif
			break;
		default:
			YDB_ERROR_CHECK(status);
			CLEANUP_AND_RETURN(status, release_ddl_lock, octo_global, locksub);
			assert(FALSE);
		}
		if (!full_auto_load_needed && !partial_auto_load_needed) {
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
	assert(full_auto_load_needed || partial_auto_load_needed);
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
	if (full_auto_load_needed) {
		/* Following call ensures that those tables which depend on functions in octo-seed.sql refer to the latest function
		 * format.
		 */
		UPGRADE_BINARY_DEFINITIONS_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
		assert(YDB_OK == status); // Ensure above call succeeded
	} else {
		assert(partial_auto_load_needed);
		/* Write the current FMT_BINARY_DEFINITION value to ^%ydboctoocto(OCTOLIT_BINFMT). So other processes do not
		 * attempt the auto upgrade as its an empty database and there is nothing to upgrade at this point.
		 */
		SET_OCTOLIT_BINFMT_GVN_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);
	}
	/* Following assert ensures that the operations in the above if/else block have succeeded.
	 * If it hasn't we should not reach this part of the code.
	 */
	assert(YDB_OK == status);
	/* Now that auto load is complete, indicate that (so other processes do not attempt the auto load)
	 * by setting ^%ydboctoocto(OCTOLIT_SEEDDFNFMT) to FMT_SEED_DEFINITION.
	 */
	fmt.len_used = snprintf(fmt.buf_addr, fmt.len_alloc, "%d", FMT_SEED_DEFINITION);
	status = ydb_set_s(&octo_global, 1, &subs, &fmt);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);

	/* Now that auto load is complete, release exclusive lock */
	status = ydb_lock_decr_s(&octo_global, 1, &locksub);
	release_ddl_lock = FALSE; /* needed so the below macro invocation does not try to release the lock again */
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, release_ddl_lock, octo_global, locksub);

	return YDB_OK;
}
