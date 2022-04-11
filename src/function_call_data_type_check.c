/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_type_check.h"
#include "function_definition_lookup.h"

#define FUNC_PARM_SEPARATOR ", " /* separator between each function parameter type in a list of type names */

/* Below is the maximum length of the string consisting of type names for each of an SQL function's parameters.
 * The type name can be "INTEGER", "NUMERIC", "VARCHAR" etc. so we use MAX_TYPE_NAME_LEN (which is asserted as an upper
 * bound for this in "get_user_visible_type_string.c") for this purpose. The maximum number of parameters for a SQL function
 * is YDB_MAX_PARMS (checked in "src/store_function_in_pg_proc.c").
 */
#define MAX_FUNC_TYPES_LEN (YDB_MAX_PARMS * (MAX_TYPE_NAME_LEN + (int)sizeof(FUNC_PARM_SEPARATOR)))

/* This function does type checking of the input function call "fc" and issues errors as appropriate.
 * At function return, the output parameter "type" holds the type of the result of the function call.
 * The input parameter "parse_context" is NULL in case of a call from "qualify_check_constraint.c" and
 * is non-NULL in case of a call from "populate_data_type.c". The input parameter "table" is non-NULL
 * in case of a call from "qualify_check_constraint.c" and NULL if called from "populate_data_type.c".
 */
int function_call_data_type_check(SqlFunctionCall *fc, SqlValueType *type, ParseContext *parse_context, SqlTable *table) {
	int result;

	assert(NULL != type);
	assert(((NULL != table) && (NULL == parse_context)) || ((NULL == table) && (NULL != parse_context)));
	result = 0;
	/* Hash the function name and parameters to determine which version of the given function is desired.
	 * Note that this hashing code flow mirrors that of "create_function_STATEMENT" in "hash_canonical_query.c",
	 * so any changes there will also need to be reflected here.
	 * Use this opportunity to also do type checking of function parameters.
	 */
	hash128_state_t state;
	HASH128_STATE_INIT(state, 0);
	assert(value_STATEMENT == fc->function_name->type);
	ADD_INT_HASH(&state, fc->function_name->type);

	SqlValue *function_name_value;
	function_name_value = fc->function_name->v.value;
	ADD_INT_HASH(&state, function_name_value->type);
	ydb_mmrhash_128_ingest(&state, (void *)function_name_value->v.reference, strlen(function_name_value->v.reference));

	char *c, function_parm_types[MAX_FUNC_TYPES_LEN];
	int   function_parm_types_len = 0;
	c = function_parm_types;

	// Identify the type of each function parameter and add it to the hash
	FunctionCallContext fc_context;
	SqlColumnList *	    cur_column_list, *start_column_list;

	cur_column_list = start_column_list = fc->parameters->v.column_list;
	fc_context.fc = fc;
	fc_context.num_args = 0;
	fc_context.num_null_args = 0;
	if (NULL != cur_column_list->value) { // Only iterate over parameters if there are any
		do {
			fc_context.num_args++;
			if (YDB_MAX_PARMS < fc_context.num_args) {
				ERROR(ERR_TOO_MANY_FUNCTION_ARGUMENTS, function_name_value->v.string_literal, YDB_MAX_PARMS);
				result = 1;
				break;
			}
			// Get type for current function argument
			if (NULL == table) {
				/* Called from "populate_data_type.c" */
				result |= populate_data_type(cur_column_list->value, type, parse_context);
			} else {
				result |= qualify_check_constraint(cur_column_list->value, table, type);
			}
			if (result) {
				break;
			}
			if (NUL_VALUE == *type) {
				/* Note down this parameter as a NULL parameter by setting a flag for it in the
				 * fc_context.null_args array.
				 */
				fc_context.null_args[fc_context.num_args - 1] = TRUE;
				/* Initialize the parameter type for this field to UNKNOWN_SqlValueType so that we can later
				 * iterate through the SqlValueType enum incrementally in function_definition_lookup().
				 */
				fc_context.arg_types[fc_context.num_args - 1] = UNKNOWN_SqlValueType;
				// Increment the number of null arguments for later reference in
				// function_definition_lookup()
				fc_context.num_null_args++;
			} else {
				// This parameter is not SQL NULL, so flag it as such and use the given type as is.
				fc_context.null_args[fc_context.num_args - 1] = FALSE;
				fc_context.arg_types[fc_context.num_args - 1] = *type;
			}
			ADD_INT_HASH(&state, *type);
			int written;
			written
			    = snprintf(c, MAX_FUNC_TYPES_LEN - function_parm_types_len, "%s", get_user_visible_type_string(*type));
			assert((MAX_FUNC_TYPES_LEN - function_parm_types_len) > written);
			c += written;
			function_parm_types_len += written;
			cur_column_list = cur_column_list->next;
			if (cur_column_list != start_column_list) {
				written = snprintf(c, MAX_FUNC_TYPES_LEN - function_parm_types_len, FUNC_PARM_SEPARATOR);
				assert((MAX_FUNC_TYPES_LEN - function_parm_types_len) > written);
				c += written;
			}
		} while (cur_column_list != start_column_list);
	} else {
		snprintf(c, MAX_FUNC_TYPES_LEN, OCTOLIT_NONE);
	}
	if (result) {
		return result;
	}

	/* In the case of a NULL value (i.e. NUL_VALUE), populate_date_type() and qualify_check_constraint will
	 * treat the NULL value as a distinct parameter type for the purposes of function definition lookup.
	 * However, M routines for SQL functions defined using non-NULL types will nonetheless accept NULL values
	 * without the need for a separate SQL function definition.
	 *
	 * To allow usage of M-level support for SQL NULL values as SQL function arguments, we must look up the
	 * relevant SQL function definition using non-NULL parameter types. This will prevent erroneous ERR_UNKNOWN_FUNCTION
	 * errors from being raised due to the treatment of NULL as a distinct type.
	 *
	 * Accordingly, when we encounter one or more NULL-valued parameters, we try to look up a function definition using all
	 * possible combinations of parameter types while excluding NUL_VALUE, for the given number of parameters.
	 *
	 * If no definition is found after attempting all possible permutations, only then do we issue ERR_UNKNOWN_FUNCTION.
	 */
	FunctionMatchContext match_context;

	memset(&match_context, 0, sizeof(match_context));
	function_definition_lookup(&fc_context, &match_context, 0);
	if (0 == match_context.num_matches) {
		// Issue syntax error and abort if function doesn't exist.
		ERROR(ERR_UNKNOWN_FUNCTION, fc->function_name->v.value->v.string_literal, function_parm_types);
		yyerror(&fc->function_name->loc, NULL, NULL, NULL, NULL, NULL);
		result = 1;
		return result;
	} else if (1 < match_context.num_matches) {
		assert(NULL != match_context.best_match);
		ERROR(ERR_FUNCTION_NOT_UNIQUE, fc->function_name->v.value->v.string_literal, function_parm_types);
		yyerror(&fc->function_name->loc, NULL, NULL, NULL, NULL, NULL);
		result = 1;
		return result;
	}
	// Update the function schema on the SqlFunctionCall to reflect the final function definition chosen
	fc_context.fc->function_schema->v.create_function = match_context.best_match;

	SqlDataType data_type;
	data_type = match_context.best_match->return_type->v.data_type_struct.data_type;
	*type = get_sqlvaluetype_from_sqldatatype(data_type, FALSE);
	return result;
}
