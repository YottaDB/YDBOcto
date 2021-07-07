/****************************************************************
 *                                                              *
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.      *
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

/* Function invoked by the rule named "function_definition" in src/parser.y (parses the CREATE FUNCTION command).
 * Returns
 *	non-NULL pointer to SqlStatement structure on success
 *	NULL on failure
 */
SqlStatement *function_definition(SqlStatement *identifier_start, SqlStatement *function_parameter_type_list,
				  SqlStatement *data_type, SqlStatement *m_function, boolean_t if_not_exists_specified) {
	SqlStatement *ret;
	SQL_STATEMENT(ret, create_function_STATEMENT);
	MALLOC_STATEMENT(ret, create_function, SqlFunction);

	SqlFunction *function;
	UNPACK_SQL_STATEMENT(function, ret, create_function);
	function->function_name = identifier_start;
	function->parameter_type_list = function_parameter_type_list;
	function->if_not_exists_specified = if_not_exists_specified;
	/* For FUNCTION return type, ignore any size specifications (i.e. if VARCHAR(30) is specified, ignore the 30).
	 * Hence only the "data_type" member is copied over below. "size" member is not copied over.
	 */
	function->return_type = data_type;
	function->extrinsic_function = m_function;

	SqlValue *function_name_value;
	UNPACK_SQL_STATEMENT(function_name_value, function->function_name, value);
	function_name_value->type = FUNCTION_NAME;

	SqlValue *extrinsic_function_value;
	UNPACK_SQL_STATEMENT(extrinsic_function_value, function->extrinsic_function, value);
	extrinsic_function_value->type = FUNCTION_NAME;

	if (NULL != function->parameter_type_list) {
		SqlParameterTypeList *start_parameter_type, *cur_parameter_type;
		UNPACK_SQL_STATEMENT(start_parameter_type, function->parameter_type_list, parameter_type_list);
		cur_parameter_type = start_parameter_type;

		// Count the number of arguments for later storage in `pg_proc`, issue error if it exceeds YDB_MAX_PARMS
		function->num_args = 0;
		do {
			function->num_args++;
			if (YDB_MAX_PARMS < function->num_args) {
				ERROR(ERR_TOO_MANY_FUNCTION_ARGUMENTS, function_name_value->v.string_literal, YDB_MAX_PARMS);
				return NULL;
			}
			cur_parameter_type = cur_parameter_type->next;
		} while (start_parameter_type != cur_parameter_type);
	}

	return ret;
}
