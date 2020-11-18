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

SqlValueType get_sqlvaluetype_from_sqldatatype(SqlDataType type, boolean_t is_unknown_type_okay) {
	switch (type) {
	case BOOLEAN_TYPE:
		return BOOLEAN_VALUE;
		break;
	case INTEGER_TYPE:
		return INTEGER_LITERAL;
		break;
	case NUMERIC_TYPE:
		return NUMERIC_LITERAL;
		break;
	case STRING_TYPE:
		return STRING_LITERAL;
		break;
	case NUL_TYPE:
		return NUL_VALUE;
		break;
	case UNKNOWN_SqlDataType:
		if (is_unknown_type_okay) {
			return UNKNOWN_SqlValueType;
			break;
		}
		// fall through
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return UNKNOWN_SqlValueType;
		break;
	}
}
