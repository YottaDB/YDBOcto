/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
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

SqlDataType get_sqldatatype_from_sqlvaluetype(SqlValueType type) {
	switch (type) {
	case BOOLEAN_VALUE:
		return BOOLEAN_TYPE;
		break;
	case INTEGER_LITERAL:
		return INTEGER_TYPE;
		break;
	case NUMERIC_LITERAL:
		return NUMERIC_TYPE;
		break;
	case STRING_LITERAL:
		return STRING_TYPE;
		break;
	case NUL_VALUE:
		return NUL_TYPE;
		break;
	default:
		/* Note: We don't expect to enter this function if the "type" is "IS_NULL_LITERAL" too.
		 * Hence we don't have a special case for this type and instead fall through to the "default:" case block.
		 */
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return UNKNOWN_SqlDataType;
		break;
	}
}
