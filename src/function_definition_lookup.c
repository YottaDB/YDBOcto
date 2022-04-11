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

void function_definition_lookup(FunctionCallContext *fc_context, FunctionMatchContext *match_context, int cur_parm) {
	SqlFunctionCall *fc;
	SqlFunction *	 function;

	fc = fc_context->fc;
	if ((0 < fc_context->num_null_args) && (cur_parm < fc_context->num_args)) {
		if (fc_context->null_args[cur_parm]) {
			/* This parameter initially had a type of NULL, but we haven't yet tried all possible types
			 * for it, so increment it to try the next type at the next recursion level.
			 */
			assert(UNKNOWN_SqlValueType == fc_context->arg_types[cur_parm]);
			while (STRING_LITERAL > fc_context->arg_types[cur_parm]) {
				fc_context->arg_types[cur_parm]++;
				function_definition_lookup(fc_context, match_context, cur_parm + 1);
			}
			/* Reset the current argument type for this parameter for possible later iteration on the parameter type of
			 * the parent parameter, i.e. cur_parm - 1. Moreover, since `UNKNOWN_SqlValueType` is not a valid type, we
			 * do not need to check for it below, so just return here.
			 */
			fc_context->arg_types[cur_parm] = UNKNOWN_SqlValueType;
		} else {
			/* This was not SQL NULL, so don't iterate over possible types. Instead, just attempt to lookup a function
			 * definition using the given type as is.
			 */
			function_definition_lookup(fc_context, match_context, cur_parm + 1);
		}
		return;
	}
	/* We've reached the tail of the recursion. Do nothing here and allow code below to do a final lookup attempt with
	 * the current parameter specifications.
	 */

	char	  function_hash[MAX_ROUTINE_LEN];
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
	for (i = 0; i < fc_context->num_args; i++) {
		ADD_INT_HASH(&state, fc_context->arg_types[i]);
	}
	generate_routine_name(&state, function_hash, sizeof(function_hash), FunctionHash);
	// Retrieve the DDL for the given function
	if (NULL == fc->function_schema) {
		// Only allocate a new function schema when there isn't one already.
		SQL_STATEMENT(fc->function_schema, create_function_STATEMENT);
	}
	/* Note that find_table() is called from the parser as every table name in the query is encountered but
	 * find_function() is not called whenever a function name is encountered in the parser. It is instead called
	 * much later from populate_data_type() after the entire query has been parsed and qualified. The reason for
	 * this is that find_function() needs type information of the actual function parameters to determine which
	 * one of the potentially many function definitions matches the current usage. And this type information is
	 * not available until all column references in the query are qualified (which only happens in
	 * qualify_query() after the entire query has been parsed and just before populate_data_type() is called).
	 */
	function = find_function(fc->function_name->v.value->v.string_literal, function_hash);
	if (NULL != function) {
		if ((NULL == match_context->best_match)
		    || (0
			!= strcmp(match_context->best_match->extrinsic_function->v.value->v.string_literal,
				  function->extrinsic_function->v.value->v.string_literal))) {
			/* If the newly matched function definition references the same extrinsic function as the current best
			 * match, then the definitions are functionally interchangable. In that case, we can accept both matches,
			 * since both will behave the same way.
			 *
			 * So, only increment match_context->num_matches when either:
			 *   1. It is the first match (NULL == match_context->best_match), OR
			 *   2. The matched functions reference *different* extrinsic functions
			 */
			match_context->num_matches++;
		}
		match_context->best_match = function;
	}

	return;
}
