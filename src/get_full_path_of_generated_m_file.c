/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include "octo.h"

/* Fills in "filename" with full path of generated M file (concatenated from "config->tmpdir" and "m_routine_name").
 * Returns
 *	0 in case of no error.
 * 	non-zero value in case of error.
 */
int get_full_path_of_generated_m_file(char *filename, int filename_len, char *m_routine_name) {
	int	    want_to_write;
	int	    status;
	size_t	    len;
	const char *tmp_dir;

	tmp_dir = config->tmp_dir;
	assert(NULL != tmp_dir);
	assert(PATH_MAX <= filename_len); /* needed by "realpath()" call below */
	if (NULL == realpath(tmp_dir, filename)) {
		status = errno;
		ERROR(ERR_SYSCALL_WITH_ARG, "realpath(tmp_dir)", status, strerror(status), tmp_dir);
		return status;
	}
	len = strlen(filename);
	assert((int)len < filename_len);
	/* Path returned by "realpath()" does not have a trailing '/' so need to add a '/' before the file name */
	want_to_write = snprintf(filename + len, filename_len - len, "/_%s.m", m_routine_name);
	if (want_to_write >= (int)(filename_len - len)) {
		ERROR(ERR_BUFFER_TOO_SMALL, "get_full_path_of_generated_m_file()");
		return ENOBUFS; /* using a system errno that closely matches the no buffer space situation */
	}
	return 0;
}
