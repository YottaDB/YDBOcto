/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
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
	case PSQL_TypeOid_int4:
		return INTEGER_LITERAL;
		break;
	case PSQL_TypeOid_numeric:
		return NUMERIC_LITERAL;
		break;
	case PSQL_TypeOid_varchar:
		return STRING_LITERAL;
		break;
	case PSQL_TypeOid_unknown:
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return UNKNOWN_SqlValueType;
		break;
	}
}
