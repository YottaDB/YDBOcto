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

/* Track the plan ("plan_filename") in Octo internal gvn ^%ydboctoocto(OCTOLIT_PLANDIRS).
 * Returns
 *	0 on success
 *	non-zero on failure
 */
int store_plandirs_gvn(char *plan_filename) {
	ydb_buffer_t plandirs_buff[4];
	char	     rtnname[MAX_ROUTINE_LEN + 1]; // Null terminator
	char	     objfilename[OCTO_PATH_MAX];
	int	     status;

	/* Record the full path of the plan srcdir and plan objdir so a later DROP TABLE or CREATE TABLE
	 * or DISCARD ALL can delete the .o file too when it deletes the .m file.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &plandirs_buff[0]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_PLANDIRS, &plandirs_buff[1]);
	YDB_STRING_TO_BUFFER(plan_filename, &plandirs_buff[2]);
	/* Derive the routine name (for the .o file name) from the tail of the .m file name (i.e. "plan_filename") */
	assert((MAX_ROUTINE_LEN + 1) == sizeof(rtnname));
	memcpy(rtnname, plan_filename + plandirs_buff[2].len_used - MAX_ROUTINE_LEN - 2, MAX_ROUTINE_LEN);
	/* - 2 to skip ".m" extension at end */
	rtnname[MAX_ROUTINE_LEN] = '\0'; /* NULL terminate as below call relies on that */
	/* The below call updates "objfilename" to be the full path including "rtnname" at the end */
	status = get_full_path_of_generated_o_file(objfilename, sizeof(objfilename), &rtnname[1]);
	if (status) {
		return 1;
	}
	YDB_STRING_TO_BUFFER(objfilename, &plandirs_buff[3]);
	/* Store gvn that links srcdir and objdir of generated plan */
	status = ydb_set_s(&plandirs_buff[0], 3, &plandirs_buff[1], NULL);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	return 0;
}
