/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"

int qualify_function_name(SqlStatement *stmt) {
	SqlValue *value;
	int name_length, result, status;
	char *c;
	ydb_buffer_t subs_array[4];
	ydb_buffer_t *octo_global, *functions_sub, *function_name_sub, *new_name_value;

	result = 0;
	UNPACK_SQL_STATEMENT(value, stmt, value);
	assert(value->type == FUNCTION_NAME);

	c = value->v.string_literal;
	name_length = strlen(value->v.string_literal);
	if(name_length > 2 && *c == '$' && *(c+1) == '$') {
		// This is a mumps expression; leave it alone
		return 0;
	}

	octo_global = &subs_array[0];
	functions_sub = &subs_array[1];
	function_name_sub = &subs_array[2];
	new_name_value = &subs_array[3];

	YDB_STRING_TO_BUFFER(config->global_names.octo, octo_global);
	YDB_LITERAL_TO_BUFFER("functions", functions_sub);
	function_name_sub->buf_addr = value->v.string_literal;
	function_name_sub->len_alloc = function_name_sub->len_used = name_length;

	YDB_MALLOC_BUFFER(new_name_value, MAX_STR_CONST);

	status = ydb_get_s(octo_global, 2, functions_sub, new_name_value);
	if(status == YDB_ERR_GVUNDEF) {
		// Not found; issue a warning and move on
		WARNING(CUSTOM_ERROR, "Unknown function: %s", value->v.string_literal);
		YDB_FREE_BUFFER(new_name_value);
		return 1;
	}
	YDB_ERROR_CHECK(status);

	// Replace the pointer in the value with the new value
	new_name_value->buf_addr[new_name_value->len_used] = '\0';
	value->v.string_literal = new_name_value->buf_addr;

	return result;
}
