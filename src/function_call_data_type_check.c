/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* This function does type checking of the input function call statement "fc_stmt" and issues errors as appropriate.
 * At function return, the output parameter "type" holds the type of the result of the function call.
 * The input parameter "parse_context" is NULL in case of a call from "qualify_check_constraint.c" and
 * is non-NULL in case of a call from "populate_data_type.c". The input parameter "table" is non-NULL
 * in case of a call from "qualify_check_constraint.c" and NULL if called from "populate_data_type.c".
 */
int function_call_data_type_check(SqlStatement *fc_stmt, SqlValueType *type, ParseContext *parse_context, SqlTable *table) {

	SqlFunctionCall *fc;
	UNPACK_SQL_STATEMENT(fc, fc_stmt, function_call);

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
	SqlColumnList	   *cur_column_list, *start_column_list;

	cur_column_list = start_column_list = fc->parameters->v.column_list;
	fc_context.fc = fc;
	fc_context.num_args = 0;
	fc_context.num_ambiguous_args = 0;
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
				result |= populate_data_type(cur_column_list->value, type, fc_stmt, parse_context, NULL);
			} else {
				result |= qualify_check_constraint(cur_column_list->value, table, type, NULL);
			}
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == *type) {
				SqlValueType fix_type;

				fix_type = STRING_LITERAL;
				if (NULL == table) {
					result
					    |= populate_data_type(cur_column_list->value, type, fc_stmt, parse_context, &fix_type);
				} else {
					result |= qualify_check_constraint(cur_column_list->value, table, type, &fix_type);
				}
				assert(!result); /* type fixing call of "populate_data_type" or "qualify_check_constraint"
						  * should never fail as it is 2nd call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type == *type);
			}

			SqlValueType lcl_type;
			lcl_type = *type;
			fc_context.arg_types[fc_context.num_args - 1] = lcl_type;
			if (IS_NUL_VALUE(lcl_type) || (INTEGER_LITERAL == lcl_type)) {
				/* A NULL actual parameter type can be matched with a function parameter type
				 * of BOOLEAN, INTEGER, NUMERIC or STRING. So treat it as an ambiguous argument.
				 * An INTEGER actual parameter type can be matched with a function parameter type
				 * of INTEGER or NUMERIC. So treat that as an ambiguous argument too.
				 */
				fc_context.num_ambiguous_args++;
			}
			ADD_INT_HASH(&state, lcl_type);
			int written;
			written = snprintf(c, MAX_FUNC_TYPES_LEN - function_parm_types_len, "%s",
					   get_user_visible_type_string(lcl_type));
			assert((MAX_FUNC_TYPES_LEN - function_parm_types_len) > written);
			c += written;
			function_parm_types_len += written;
			cur_column_list = cur_column_list->next;
			if (cur_column_list != start_column_list) {
				written = snprintf(c, MAX_FUNC_TYPES_LEN - function_parm_types_len, FUNC_PARM_SEPARATOR);
				assert((MAX_FUNC_TYPES_LEN - function_parm_types_len) > written);
				c += written;
				function_parm_types_len += written;
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

	memset(&match_context, 0, sizeof(match_context)); /* clears "match_context.num_matches" and "match_context.best_match" */

	int ret;
	ret = function_definition_lookup(&fc_context, &match_context);
	if (0 != ret) {
		result = 1;
		return result;
	}
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
