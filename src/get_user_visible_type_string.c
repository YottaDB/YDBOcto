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

char *get_user_visible_type_string(SqlValueType type) {
	switch(type) {
	case NUMERIC_LITERAL:
		return "NUMERIC";
	case INTEGER_LITERAL:
		return "INTEGER";
	case STRING_LITERAL:
		return "VARCHAR";	/* VARCHAR (not STRING) is the externally visible type name in SQL */
	case BOOLEAN_VALUE:
		return "BOOLEAN";
	case PARAMETER_VALUE:
		return "PARAMETER";
	case COLUMN_REFERENCE:
	case CALCULATED_VALUE:
	case UNKNOWN_SqlValueType:
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		break;
	}
	return "";
}
