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
#include "octo_type_check.h"

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

	SqlValue *value;
	value = fc->function_name->v.value;
	ADD_INT_HASH(&state, value->type);
	ydb_mmrhash_128_ingest(&state, (void *)value->v.reference, strlen(value->v.reference));

	char *c, function_parm_types[MAX_FUNC_TYPES_LEN];
	int   function_parm_types_len = 0;
	c = function_parm_types;

	// Identify the type of each function parameter and add it to the hash
	SqlColumnList *cur_column_list, *start_column_list;
	cur_column_list = start_column_list = fc->parameters->v.column_list;
	if (NULL != cur_column_list->value) { // Only iterate over parameters if there are any
		do {
			if (NULL == table) {
				/* Called from "populate_data_type.c" */
				result |= populate_data_type(cur_column_list->value, type, parse_context);
			} else {
				result |= qualify_check_constraint(cur_column_list->value, table, type);
			}
			if (result) {
				break;
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

	char function_hash[MAX_ROUTINE_LEN];
	int  status;
	status = generate_routine_name(&state, function_hash, sizeof(function_hash), FunctionHash);
	if (status) {
		result = 1;
		return result;
	}
	// Retrieve the DDL for the given function
	SQL_STATEMENT(fc->function_schema, create_function_STATEMENT);
	/* Note that find_table() is called from the parser as every table name in the query is encountered but
	 * find_function() is not called whenever a function name is encountered in the parser. It is instead called
	 * much later from populate_data_type() after the entire query has been parsed and qualified. The reason for this is
	 * that find_function() needs type information of the actual function parameters to determine which one of the
	 * potentially many function definitions matches the current usage. And this type information is not available until
	 * all column references in the query are qualified (which only happens in qualify_query() after the entire query
	 * has been parsed and just before populate_data_type() is called).
	 */
	SqlFunction *function;
	function = fc->function_schema->v.create_function
	    = find_function(fc->function_name->v.value->v.string_literal, function_hash);
	// Issue syntax error and abort if function doesn't exist. UNKNOWN_FUNCTION error will be issued by find_function.
	if (NULL == function) {
		ERROR(ERR_UNKNOWN_FUNCTION, fc->function_name->v.value->v.string_literal, function_parm_types);
		yyerror(&fc->function_name->loc, NULL, NULL, NULL, NULL, NULL);
		result = 1;
		return result;
	}

	SqlDataType data_type;
	data_type = function->return_type->v.data_type_struct.data_type;
	*type = get_sqlvaluetype_from_sqldatatype(data_type, FALSE);
	return result;
}
