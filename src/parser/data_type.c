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
#include <stdlib.h>

#include "octo.h"
#include "octo_types.h"

// Function invoked by the rule named "data_type" in src/parser.y
SqlStatement *data_type(SqlDataType data_type, SqlStatement *size_or_precision, SqlStatement *scale) {
	SqlStatement *ret;

	SQL_STATEMENT(ret, data_type_struct_STATEMENT);
	ret->v.data_type_struct.data_type = data_type;
	if (NULL == size_or_precision) {
		/* Column SIZE or PRECISION not specified. Set field accordingly. */
		ret->v.data_type_struct.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;
	} else {
		/* Even though "atoi()" does not do error checking like "strtol()", it is fine here as most error
		 * checking is already done by the "ddl_int_literal_value" rule in "src/parser.y" (asserted below).
		 */
		assert((value_STATEMENT == size_or_precision->type) && (NUMERIC_LITERAL == size_or_precision->v.value->type)
		       && size_or_precision->v.value->is_int);
		ret->v.data_type_struct.size_or_precision = atoi(size_or_precision->v.value->v.string_literal);
	}
	if (NULL == scale) {
		/* Column SCALE not specified. Set field accordingly. */
		ret->v.data_type_struct.scale = SCALE_UNSPECIFIED;
	} else {
		assert(NULL != size_or_precision);
		/* Even though "atoi()" does not do error checking like "strtol()", it is fine here as most error
		 * checking is already done by the "ddl_literal_value" rule in "src/parser.y" (asserted below).
		 */
		assert((value_STATEMENT == scale->type) && (NUMERIC_LITERAL == scale->v.value->type) && scale->v.value->is_int);
		ret->v.data_type_struct.scale = atoi(scale->v.value->v.string_literal);
	}
	return ret;
}
