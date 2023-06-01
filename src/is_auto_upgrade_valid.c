/****************************************************************
 *								*
 * Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	*
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

int is_auto_upgrade_valid(void) {
	ydb_buffer_t octo_global, subs, fmt;
	char	     fmt_buff[INT32_TO_STRING_MAX];
	int	     status;
#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
	int binary_definition_fmt;
#endif
	boolean_t auto_upgrade_can_be_done;

#ifdef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
	if (NULL != getenv("disable_auto_upgrade")) {
		/* Caller set an env var to indicate auto upgrade has to be disabled (e.g. TMU02 bats subtest). Skip it. */
		return YDB_OK;
	}
#endif
	/* Check if binary definitions (tables or functions) can be auto upgraded. They can be if
	 * ^%ydboctoocto(OCTOLIT_BINFMT) doesn't exist or has a value less than or equal to FMT_BINARY_DEFINITION.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(OCTOLIT_BINFMT, &subs);
	fmt.buf_addr = &fmt_buff[0];
	fmt.len_alloc = sizeof(fmt_buff);
	status = ydb_get_s(&octo_global, 1, &subs, &fmt);
	switch (status) {
	case YDB_ERR_GVUNDEF:
		auto_upgrade_can_be_done = TRUE;
		break;
	case YDB_OK:
		assert(fmt.len_used < sizeof(fmt_buff));
		fmt.buf_addr[fmt.len_used] = '\0';
#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
		binary_definition_fmt = atoi(fmt.buf_addr);
		auto_upgrade_can_be_done = (FMT_BINARY_DEFINITION >= binary_definition_fmt);
#else
		auto_upgrade_can_be_done = TRUE;
#endif
		break;
	default:
		YDB_ERROR_CHECK(status);
		return status;
		break;
	}
	if (auto_upgrade_can_be_done) {
		// Binary definition format is in the valid range
		return YDB_OK;
	} else {
		/* Auto upgrade cannot be done */
		ERROR(ERR_AUTO_UPGRADE_DB_HIGHER_FMT, "");
		return 1;
	}
}
