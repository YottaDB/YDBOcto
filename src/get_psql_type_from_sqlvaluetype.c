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

PSQL_TypeOid get_psql_type_from_sqlvaluetype(SqlValueType type) {
	switch (type) {
	case BOOLEAN_VALUE:
		return PSQL_TypeOid_bool;
		break;
	case INTEGER_LITERAL:
		return PSQL_TypeOid_int4;
		break;
	case NUMERIC_LITERAL:
		return PSQL_TypeOid_numeric;
		break;
	case STRING_LITERAL:
		return PSQL_TypeOid_varchar;
		break;
	case PARAMETER_VALUE:
		/* Needed for extended query case where we generate a plan without knowing the type of one or more literal
		 * parameters. Since these are inferred at Bind time rather than at Parse time (when the plan is generated), we
		 * cannot specify a concrete type. However, we also don't want plan generation to fail for this reason, so simply
		 * specify that the type is unknown until it is latter inferred from concrete values.
		 */
		return PSQL_TypeOid_unknown;
		break;
	case NUL_VALUE:
		return PSQL_TypeOid_unknown;
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return PSQL_TypeOid_unknown;
		break;
	}
}
