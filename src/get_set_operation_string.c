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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

char *get_set_operation_string(SqlSetOperationType type) {
	switch (type) {
	case SET_UNION:
		return "UNION";
		break;
	case SET_UNION_ALL:
		return "UNION ALL";
		break;
	case SET_EXCEPT:
		return "EXCEPT";
		break;
	case SET_EXCEPT_ALL:
		return "EXCEPT ALL";
		break;
	case SET_INTERSECT:
		return "INTERSECT";
		break;
	case SET_INTERSECT_ALL:
		return "INTERSECT ALL";
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		break;
	}
	return "";
}
