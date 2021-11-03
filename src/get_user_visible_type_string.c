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

char *get_user_visible_type_string(SqlValueType type) {
	char *ret;

	switch (type) {
	case BOOLEAN_VALUE:
		ret = "BOOLEAN";
		break;
	case NUMERIC_LITERAL:
		ret = "NUMERIC";
		break;
	case INTEGER_LITERAL:
		ret = "INTEGER";
		break;
	case STRING_LITERAL:
		ret = "VARCHAR"; /* VARCHAR (not STRING) is the externally visible type name in SQL */
		break;
	case TABLE_ASTERISK:
		ret = "TABLENAME.*";
		break;
	case PARAMETER_VALUE:
		ret = "PARAMETER";
		break;
	case NUL_VALUE:
		ret = "NULL";
		break;
	case COLUMN_REFERENCE:
	case CALCULATED_VALUE:
	case UNKNOWN_SqlValueType:
	case IS_NULL_LITERAL:
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		ret = "";
		break;
	}
	assert(MAX_TYPE_NAME_LEN > strlen(ret)); /* relied upon by users of MAX_TYPE_NAME_LEN (e.g. populate_data_type.c) */
	return ret;
}
