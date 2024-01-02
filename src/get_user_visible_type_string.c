/****************************************************************
 *								*
 * Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	*
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
	case BOOLEAN_OR_STRING_LITERAL:
		/* It is possible for BOOLEAN_OR_STRING_LITERAL to be unresolved to a specific type at this point.
		 * A `\d view_name` for any of the below views can get us here.
		 * 	create view view_name as `select 't';
		 *	create view view_name as select true,'t' union select 'f','t';
		 * In the second create statement above the second column in both parts of the set operation will be of
		 * `BOOLEAN_OR_STRING_LITERAL` type. This will be resolved in hash_canonical_query() to be of type
		 * STRING_LITERAL. So treat this as a STRING_LITERAL.
		 */
		/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
		/* fall through */
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
	case DATE_LITERAL:
		ret = "DATE";
		break;
	case TIME_LITERAL:
		ret = "TIME";
		break;
	case TIME_WITH_TIME_ZONE_LITERAL:
		ret = "TIME WITH TIME ZONE";
		break;
	case TIMESTAMP_LITERAL:
		ret = "TIMESTAMP";
		break;
	case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
		ret = "TIMESTAMP WITH TIME ZONE";
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
