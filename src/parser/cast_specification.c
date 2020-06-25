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

// Function invoked by the rule named "table_reference" in src/parser/select.y
SqlStatement *cast_specification(SqlStatement *cast_specification, SqlStatement *source) {
	SqlValueType  type;
	SqlStatement *ret;

	type = (SqlValueType)cast_specification;
	if (INVALID_SqlValueType == type) {
		ret = NULL;
	} else {
		SqlValue *value;

		SQL_STATEMENT(ret, value_STATEMENT);
		MALLOC_STATEMENT(ret, value, SqlValue);
		UNPACK_SQL_STATEMENT(value, ret, value);
		value->type = COERCE_TYPE;
		value->coerced_type = type;
		/* value->pre_coerced_type will be initialized in populate_data_type */
		value->v.coerce_target = source;
	}
	return ret;
}
