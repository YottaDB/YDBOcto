/****************************************************************
 *								*
 * Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "logical_plan.h"

boolean_t is_keycol_type_string(SqlKey *key) {
	boolean_t is_string;

	switch (key->column->data_type_struct.data_type) {
	case BOOLEAN_TYPE:
	case INTEGER_TYPE:
	case NUMERIC_TYPE:
	case DATE_TYPE:
	case TIME_TYPE:
	case TIME_WITH_TIME_ZONE_TYPE:
	case TIMESTAMP_TYPE:
	case TIMESTAMP_WITH_TIME_ZONE_TYPE:
		is_string = FALSE;
		break;
	case STRING_TYPE:
		is_string = TRUE;
		break;
	case UNKNOWN_SqlDataType:
	case NUL_TYPE:
	default:
		assert(FALSE);
		is_string = FALSE; /* initialize variable to avoid possible compiler warning */
		break;
	}
	return is_string;
}
