/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Returns 0 on success. 1 on failure. */
int function_definition_lookup(FunctionCallContext *fc_context, FunctionMatchContext *match_context) {
	SqlFunctionCall *fc;
	SqlFunction *	 function;

	fc = fc_context->fc;

	char	  function_hash[MAX_ROUTINE_LEN + 1];
	int	  i;
	SqlValue *function_name_value;

	// Initialize the hash before attempting to look up function
	hash128_state_t state;
	HASH128_STATE_INIT(state, 0);
	assert(value_STATEMENT == fc_context->fc->function_name->type);
	ADD_INT_HASH(&state, fc->function_name->type);
	function_name_value = fc->function_name->v.value;
	ADD_INT_HASH(&state, function_name_value->type);
	ydb_mmrhash_128_ingest(&state, (void *)function_name_value->v.reference, strlen(function_name_value->v.reference));

	// Add each parameter type to the hash
	int actual_args;

	actual_args = fc_context->num_args;
	for (i = 0; i < actual_args; i++) {
		SqlValueType arg_type;

		arg_type = fc_context->arg_types[i];
		if (NUL_VALUE == arg_type) {
			/* Postgres treats NULL as VARCHAR by default. If that does not work, then it tries out
			 * all other type matches. See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_887646106.
			 */
			assert(0 != fc_context->num_ambiguous_args);
			arg_type = STRING_LITERAL;
		}
		ADD_INT_HASH(&state, arg_type);
	}
	generate_name_type(FunctionHash, &state, 0, function_hash, sizeof(function_hash));
	// Retrieve the DDL for the given function
	if (NULL == fc->function_schema) {
		// Only allocate a new function schema when there isn't one already.
		SQL_STATEMENT(fc->function_schema, create_function_STATEMENT);
		/* Note: "function_schema" is further initialized in the caller "function_call_data_type_check.c". */
	}
	/* Note that find_view_or_table() is called from the parser as every table name in the query is encountered but
	 * find_function() is not called whenever a function name is encountered in the parser. It is instead called
	 * much later from populate_data_type() after the entire query has been parsed and qualified. The reason for
	 * this is that find_function() needs type information of the actual function parameters to determine which
	 * one of the potentially many function definitions matches the current usage. And this type information is
	 * not available until all column references in the query are qualified (which only happens in
	 * qualify_query() after the entire query has been parsed and just before populate_data_type() is called).
	 */
	char *function_name;
	function_name = fc->function_name->v.value->v.string_literal;
	function = find_function(function_name, function_hash);
	if (NULL != function) {
		assert(NULL == match_context->best_match);
		match_context->best_match = function;
		match_context->num_matches++;
	}
	if ((0 == fc_context->num_ambiguous_args) || (0 != match_context->num_matches)) {
		assert(1 >= match_context->num_matches);
		return 0;
	}
	/* We did not find a function match and there are actual parameters which are ambiguous.
	 * Examine all existing function prototypes with the same name and see how many of them match.
	 */
	ydb_buffer_t octo_global, function_subs[3];

	assert(0 != actual_args);
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_subs[0]);
	YDB_STRING_TO_BUFFER(function_name, &function_subs[1]);
	function_subs[2].len_alloc = sizeof(function_hash);
	function_subs[2].len_used = 0;
	function_subs[2].buf_addr = function_hash;
	while (TRUE) {
		int status;
		status = ydb_subscript_next_s(&octo_global, 3, &function_subs[0], &function_subs[2]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
		function = find_function(function_name, function_hash);
		if (NULL == function) {
			return 1;
		}
		/* Check if the parameter list of this function matches the actual parameters */
		SqlParameterTypeList *parameter_type_list;
		SqlParameterTypeList *cur_parameter_type_list;

		if (NULL == function->parameter_type_list) {
			/* Function prototype allows for 0 parameters whereas we know actual parameters "actual_args"
			 * is non-zero (asserted above). This cannot be a match. Move on to next function.
			 */
			continue;
		}
		UNPACK_SQL_STATEMENT(parameter_type_list, function->parameter_type_list, parameter_type_list);
		cur_parameter_type_list = parameter_type_list;
		int	  prototype_args = 0;
		boolean_t match = TRUE;
		do {
			SqlDataType  data_type;
			SqlValueType value_type;

			assert(data_type_struct_STATEMENT == cur_parameter_type_list->data_type_struct->type);
			data_type = cur_parameter_type_list->data_type_struct->v.data_type_struct.data_type;
			value_type = get_sqlvaluetype_from_sqldatatype(data_type, FALSE);

			/* If actual parameter is NULL literal, it will match with ANY function parameter type.
			 * If actual parameter is INTEGER literal, it will match with INTEGER or NUMERIC function
			 * parameter type.
			 * Otherwise, actual parameter type should match with function parameter type.
			 */
			SqlValueType arg_type;
			if (prototype_args >= actual_args) {
				/* Function prototype requires more parameters than actual parameters.
				 * This function cannot be a match. Move on to next function.
				 */
				match = FALSE;
				break;
			}
			arg_type = fc_context->arg_types[prototype_args];
			switch (arg_type) {
			case NUL_VALUE:
				assert(TRUE == match);
				break;
			case INTEGER_LITERAL:
				match = ((INTEGER_LITERAL == value_type) || (NUMERIC_LITERAL == value_type));
				break;
			default:
				match = (arg_type == value_type);
				break;
			}
			if (!match) {
				break;
			}
			cur_parameter_type_list = cur_parameter_type_list->next;
			prototype_args++;
		} while (cur_parameter_type_list != parameter_type_list);
		if (!match) {
			continue; /* move on to next function to see match */
		}
		if (prototype_args != actual_args) {
			continue; /* move on to next function to see match */
		}
		if ((NULL == match_context->best_match)
		    || (0
			!= strcmp(match_context->best_match->extrinsic_function->v.value->v.string_literal,
				  function->extrinsic_function->v.value->v.string_literal))) {
			/* If the newly matched function definition references the same extrinsic function as the current
			 * best match, then the definitions are functionally interchangable. In that case, we can accept
			 * both matches, since both will behave the same way.
			 *
			 * So, only increment match_context->num_matches when either:
			 *   1. It is the first match (NULL == match_context->best_match), OR
			 *   2. The matched functions reference *different* extrinsic functions
			 */
			match_context->num_matches++;
			if (1 < match_context->num_matches) {
				return 0; /* So caller can issue ERR_FUNCTION_NOT_UNIQUE error */
			}
			match_context->best_match = function;
		}
	}
	return 0;
}
