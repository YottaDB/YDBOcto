/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
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

SqlValueType get_sqlvaluetype_from_psql_type(PSQL_TypeOid type) {
	switch (type) {
	case PSQL_TypeOid_int2:
	case PSQL_TypeOid_int4:
	case PSQL_TypeOid_int8:
		return INTEGER_LITERAL;
		break;
	case PSQL_TypeOid_numeric:
		return NUMERIC_LITERAL;
		break;
	case PSQL_TypeOid_varchar:
		return STRING_LITERAL;
		break;
	case PSQL_TypeOid_date:
		return DATE_LITERAL;
		break;
	case PSQL_TypeOid_time:
		return TIME_LITERAL;
		break;
	case PSQL_TypeOid_timetz:
		return TIME_WITH_TIME_ZONE_LITERAL;
		break;
	case PSQL_TypeOid_timestamp:
		return TIMESTAMP_LITERAL;
		break;
	case PSQL_TypeOid_timestamptz:
		return TIMESTAMP_WITH_TIME_ZONE_LITERAL;
		break;
	case PSQL_TypeOid_unknown:
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return UNKNOWN_SqlValueType;
		break;
	}
}
