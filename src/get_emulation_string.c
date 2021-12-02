/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
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

char *get_emulation_string() {
	switch (config->database_emulation) {
	case POSTGRES:
		assert(MAX_EMULATION_STRING_LEN > strlen(OCTOLIT_POSTGRESQL));
		return OCTOLIT_POSTGRESQL;
		break;
	case MYSQL:
		assert(MAX_EMULATION_STRING_LEN > strlen(OCTOLIT_MYSQL));
		return OCTOLIT_MYSQL;
		break;
	default:
		assert(FALSE);
		return NULL;
		break;
	}
}
