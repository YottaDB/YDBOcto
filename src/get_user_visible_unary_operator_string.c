/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Returns a string corresponding to the unary operator (input parameter "operation") */
char *get_user_visible_unary_operator_string(enum UnaryOperations operation) {
	char *ret;

	switch (operation) {
	case FORCE_NUM:
		ret = "+";
		break;
	case NEGATIVE:
		ret = "-";
		break;
	case BOOLEAN_NOT:
		ret = "NOT";
		break;
	case BOOLEAN_EXISTS:
		ret = "EXISTS";
		break;
	case BOOLEAN_NOT_EXISTS:
		assert(FALSE); /* since this is unused per comment in "UnaryOperations" structure in "src/octo_types.h" */
		ret = "";
		break;
	default:
		/* All valid code paths are already enumerated above. So this should be an impossible code path.
		 * But is there to avoid false [-Wmaybe-uninitialized] warnings from compiler.
		 */
		assert(FALSE);
		ret = "";
		break;
	}
	return ret;
}
