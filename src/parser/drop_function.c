/****************************************************************
 *                                                              *
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.      *
 * All rights reserved.                                         *
 *                                                              *
 *      This source code contains the intellectual property     *
 *      of its copyright holder(s), and is made available       *
 *      under a license.  If you do not know the terms of       *
 *      the license, please stop and do not read further.       *
 *                                                              *
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Function invoked by the rule named "drop_function" in src/parser/drop.y (parses the DROP FUNCTION command).
 *
 * The function_parameter_type_list argument accepts $optional_function_parameter_type_list argument from parser
 * since its value can be NULL.
 *
 * Returns
 *	non-NULL pointer to SqlStatement structure on success
 *	NULL on failure
 */
SqlStatement *drop_function(SqlStatement *identifier, SqlStatement *function_parameter_type_list, boolean_t if_exists_specified) {
	SqlStatement *ret;
	SQL_STATEMENT(ret, drop_function_STATEMENT);
	MALLOC_STATEMENT(ret, drop_function, SqlDropFunctionStatement);

	SqlDropFunctionStatement *drop_function;
	UNPACK_SQL_STATEMENT(drop_function, ret, drop_function)
	drop_function->function_name = identifier;
	drop_function->parameter_type_list = function_parameter_type_list;
	drop_function->if_exists_specified = if_exists_specified;

	SqlValue *function_name_value;
	UNPACK_SQL_STATEMENT(function_name_value, drop_function->function_name, value);
	function_name_value->type = FUNCTION_NAME;

	return ret;
}
