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
	SqlStatement *ret;

	if (NULL == cast_specification) {
		ret = NULL;
	} else {
		SqlValue *value;

		SQL_STATEMENT(ret, value_STATEMENT);
		MALLOC_STATEMENT(ret, value, SqlValue);
		UNPACK_SQL_STATEMENT(value, ret, value);
		value->type = COERCE_TYPE;
		assert(data_type_struct_STATEMENT == cast_specification->type);
		value->coerced_type = cast_specification->v.data_type_struct;
		/* value->pre_coerced_type will be initialized in populate_data_type */
		value->v.coerce_target = source;
	}
	return ret;
}
