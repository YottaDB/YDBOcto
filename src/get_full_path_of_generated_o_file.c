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

#include <string.h>
#include <errno.h>
#include <assert.h>

#include "octo.h"

/* Fills in "filename" with full path of .o file corresponding to generated M file (concatenated from "config->plan_obj_dir"
 * and "o_routine_name" and a ".o" extension). This is very similar to "get_full_path_of_generated_o_file.c".
 * Returns
 *	0 in case of no error.
 * 	non-zero value in case of error.
 */
int get_full_path_of_generated_o_file(char *filename, int filename_len, char *o_routine_name) {
	int	    want_to_write;
	const char *plan_obj_dir;

	plan_obj_dir = config->plan_obj_dir;
	assert(NULL != plan_obj_dir);
	assert('/' == plan_obj_dir[0]);	  /* assert that the "realpath()" call has already happened (in octo_init.c) */
	assert(PATH_MAX <= filename_len); /* "realpath()" (would have happened already) expects second
					   * parameter to have at least a length of PATH_MAX and so we
					   * assert that "filename_len" agrees with that.
					   */
	assert(MAX_ROUTINE_LEN > strlen(o_routine_name));
	/* Path returned by "realpath()" does not have a trailing '/' so need to add a '/' before the file name */
	want_to_write = snprintf(filename, filename_len, "%s/_%s.o", plan_obj_dir, o_routine_name);
	if (want_to_write >= (int)filename_len) {
		assert(FALSE);
		ERROR(ERR_BUFFER_TOO_SMALL, "get_full_path_of_generated_o_file()");
		return ENOBUFS; /* using a system errno that closely matches the no buffer space situation */
	}
	return 0;
}
