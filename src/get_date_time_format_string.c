/****************************************************************
 *								*
 * Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_types.h"

char *get_date_time_format_string(enum OptionalKeyword keyword) {
	switch (keyword) {
	case OPTIONAL_DATE_TIME_FILEMAN:
		return "FILEMAN";
		break;
	case OPTIONAL_DATE_TIME_HOROLOG:
		return "HOROLOG";
		break;
	case OPTIONAL_DATE_TIME_ZHOROLOG:
		return "ZHOROLOG";
		break;
	case OPTIONAL_DATE_TIME_ZUT:
		return "ZUT";
		break;
	case OPTIONAL_DATE_TIME_TEXT:
		/* fall through */
	default:
		assert(FALSE);
		return "";
		break;
	}
}
