/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
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
	SqlValue *    value;

	if (NULL == cast_specification) {
		ret = NULL;
	} else {
		switch (cast_specification->type) {
		case value_STATEMENT:
			/* This is a "::regclass" or "::regproc" usage. The full fledged implementation of these alias types
			 * will be done in YDBOcto#967. Until then implement just the "::" operator on these types by
			 * invoking an M function. So replace the type cast usage with an equivalent M function call.
			 */
			assert(FUNCTION_NAME == cast_specification->v.value->type);
			SQL_STATEMENT(ret, value_STATEMENT);
			MALLOC_STATEMENT(ret, value, SqlValue);

			SqlStatement *	 fc_statement;
			SqlFunctionCall *fc;
			UNPACK_SQL_STATEMENT(value, ret, value);
			value->type = CALCULATED_VALUE;
			SQL_STATEMENT(fc_statement, function_call_STATEMENT);
			value->v.calculated = fc_statement;
			MALLOC_STATEMENT(fc_statement, function_call, SqlFunctionCall);
			UNPACK_SQL_STATEMENT(fc, fc_statement, function_call);
			fc->function_name = cast_specification;
			fc->parameters = create_sql_column_list(source, NULL, NULL);
			break;
		default:
			assert(data_type_struct_STATEMENT == cast_specification->type);
			SQL_STATEMENT(ret, value_STATEMENT);

			MALLOC_STATEMENT(ret, value, SqlValue);
			UNPACK_SQL_STATEMENT(value, ret, value);
			value->type = COERCE_TYPE;
			value->u.coerce_type.coerced_type = cast_specification->v.data_type_struct;
			/* value->u.coerce_type.pre_coerced_type will be initialized in populate_data_type */
			value->v.coerce_target = source;
			break;
		}
	}
	return ret;
}
