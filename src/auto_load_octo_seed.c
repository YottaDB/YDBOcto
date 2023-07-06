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
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h> /* for "wait()" call */
#include <unistd.h>
#include <libgen.h> /* for "dirname()" call */
#include <assert.h>
#include <errno.h>

#include "octo.h"

/* This macro was previously declared in octo.h, but was obsoleted by the #597 fixes. It is used here simply to clean up any
 * obsoleted nodes stored in the database prior to these fixes.
 */
#define OCTOLIT_VARIABLES "variables"

/* Loads the current "octo-seed.sql" and "octo-seed.zwr" files.
 * Called from "auto_load_octo_seed_if_needed()" only if a need for this is determined (i.e. first use of new Octo build).
 * Returns 0 on success and 1 on error.
 */
int auto_load_octo_seed(void) {
	char	     octo_seed_path[OCTO_PATH_MAX], exe_path[OCTO_PATH_MAX], mupip_path[OCTO_PATH_MAX];
	int	     status;
	size_t	     filename_len;
	pid_t	     child_id;
	char *	     ydb_dist;
	ydb_buffer_t variable_buffers[2];

	/* Get value of "ydb_dist" env var first */
	ydb_dist = getenv("ydb_dist");
	if (NULL == ydb_dist) {
		ERROR(ERR_FAILED_TO_RETRIEVE_ENVIRONMENT_VARIABLE, "ydb_dist");
		assert(FALSE);
		return 1;
	}

	YDB_STRING_TO_BUFFER(config->global_names.octo, &variable_buffers[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &variable_buffers[1]);
	status = ydb_delete_s(&variable_buffers[0], 1, &variable_buffers[1], YDB_DEL_TREE);
	UNUSED(status);
	assert(YDB_OK == status);

	/* -----------------------------------------------------------------
	 * Do the actual load of the "octo-seed.sql" file.
	 * -----------------------------------------------------------------
	 */
	/* Towards that first determine the location of the octo-seed.sql file.
	 * Note: Lot of the code here is similar to that in "octo_init.c" to determine path of "ydbocto.ci".
	 */
	if (!DISABLE_INSTALL) {
		/* Octo was installed under $ydb_dist (using "cmake -D DISABLE_INSTALL=OFF"). So pick that path.
		 * NOTE: this uses a hard-coded path under $ydb_dist. Not the currently active $ydb_ci.
		 */
		int len;

		len = snprintf(octo_seed_path, sizeof(octo_seed_path), "%s/plugin/octo/octo-seed.sql", ydb_dist);
		if ((int)sizeof(octo_seed_path) <= len) {
			ERROR(ERR_BUFFER_TOO_SMALL, "Octo seed-file path : $ydb_dist/plugin/octo/octo-seed.sql");
			assert(FALSE);
			return 1;
		}
	} else {
		/* Octo was built but not installed. So derive the path of "octo-seed.sql" from the path of the
		 * octo/rocto binary that is currently running.
		 */
		ssize_t	    exe_path_len;
		const char *src_path;

		exe_path_len = readlink("/proc/self/exe", exe_path, OCTO_PATH_MAX);
		if ((-1 != exe_path_len) && (OCTO_PATH_MAX > exe_path_len)) {
			exe_path[exe_path_len] = '\0'; // readlink() doesn't add a null terminator per man page
			src_path = dirname(exe_path);
			if (NULL != src_path) {
				int len;

				len = snprintf(octo_seed_path, sizeof(octo_seed_path), "%s/octo-seed.sql", src_path);
				if ((int)sizeof(octo_seed_path) <= len) {
					ERROR(ERR_BUFFER_TOO_SMALL, "octo-seed.sql seed file path");
					assert(FALSE);
					return 1;
				}
			} else {
				ERROR(ERR_LIBCALL_WITH_ARG, "dirname()", exe_path);
				assert(FALSE);
				return 1;
			}
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "readlink()", "/proc/self/exe");
			assert(FALSE);
			return 1;
		}
	}
	status = run_query_file(octo_seed_path);
	if (0 != status) {
		/* run_query() has failed to process one of the seed table or function. Exit with an error. */
		ERROR(ERR_AUTO_SEED_LOAD, "");
		return 1;
	}
	/* -----------------------------------------------------------------
	 * Do the actual load of the "octo-seed.zwr" file.
	 * -----------------------------------------------------------------
	 */
	/* "$ydb_dist/mupip" does the load. Use "fork()"/"exec()" to do it. */
	child_id = fork();
	if (0 != child_id) {
		/* Parent */
		pid_t wait_pid;
		int   childExitStatus;

		if (0 > child_id) {
			/* Error in "fork" */
			ERROR(ERR_SYSCALL, "fork()", errno, strerror(errno));
			assert(FALSE);
			return 1;
		}
		do {
			/* Handle possibility of EINTR */
			wait_pid = wait(&childExitStatus);
			if ((-1 != wait_pid) || (EINTR != errno)) {
				break;
			}
		} while (TRUE);
		if (0 > wait_pid) {
			ERROR(ERR_SYSCALL, "wait()", errno, strerror(errno));
			assert(FALSE);
			return 1;
		}
		if (WIFEXITED(childExitStatus)) {
			/* Get exit status of child */
			status = WEXITSTATUS(childExitStatus);
		} else {
			/* Child did not exit. Not sure what happened. Just copy exit status as is. */
			status = childExitStatus;
		}
	} else {
		/* Child */
		int   len;
		char *param0 = "mupip";
		char *param1 = "load";
		char *param2 = "-ignorechset"; /* needed to avoid LOADINVCHSET error in case ydb_chset="UTF-8" */

		/* Note: This is the child process whose only purpose is to execute the "mupip load".
		 * In case of any error, print the error and exit the child process.
		 * We do not want to "return" as that would go back to the caller function and continue with octo
		 * which is not what we want to do in this child process. Hence the "exit" usages below with an abnormal status.
		 */
		len = snprintf(mupip_path, sizeof(mupip_path), "%s/mupip", ydb_dist);
		if ((int)sizeof(mupip_path) <= len) {
			ERROR(ERR_BUFFER_TOO_SMALL, "$ydb_dist/mupip");
			assert(FALSE);
			exit(127);
		}
		/* Determine the full path of the "octo-seed.zwr" file.
		 * This is the same as the full path of the "octo-seed.sql" file.
		 * So just overwrite the "sql" portion with "zwr".
		 */
		filename_len = strlen(octo_seed_path);
		assert(!memcmp(octo_seed_path + filename_len - 3, "sql", 3));
		memcpy(octo_seed_path + filename_len - 3, "zwr", 3);
		/* mupip prints load output to stderr even on a successful load but we do not want that to show up in the
		 * Octo output so close stderr (i.e. file descriptor 2).
		 */
		close(2);
		execl(mupip_path, param0, param1, param2, octo_seed_path, NULL);
		/* If we come here, it means "execl()" returned back to us. This is an error scenario.
		 * This is one more reason why we don't check the return status of the "execl()" call above.
		 */
		assert(FALSE);
		exit(127);
	}
	return status;
}
