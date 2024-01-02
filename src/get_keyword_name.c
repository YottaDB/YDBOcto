/****************************************************************
 *								*
 * Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Given a keyword type enum as input, this function returns the keyword name as a string */
char *get_keyword_name(OptionalKeyword keyword) {
	char *ret;

	switch (keyword) {
	case OPTIONAL_BOOLEAN_EXPANSION:
		ret = "BOOLEAN_EXPANSION";
		break;
	case OPTIONAL_DEFAULT:
		ret = "DEFAULT";
		break;
	case OPTIONAL_DISTINCT:
		ret = "DISTINCT";
		break;
	case OPTIONAL_XREF_INDEX:
		ret = "XREF_INDEX";
		break;
	default:
		assert(FALSE);
		ret = NULL;
		break;
	}
	return ret;
}
