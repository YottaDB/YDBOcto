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

#define	ABS_FUNCTION_SQL_NAME	"ABS"
#define	ABS_FUNCTION_OCTO_NAME	"$$ABS^%ydboctosqlfunctions"

int qualify_function_name(SqlStatement *stmt) {
	SqlValue	*value;
	char		*c;
	int		name_length, result, status;
	ydb_buffer_t	*octo_global, *functions_sub, *function_name_sub, *new_name_value;
	ydb_buffer_t	subs_array[4];
	size_t		alloc_len;

	result = 0;
	UNPACK_SQL_STATEMENT(value, stmt, value);
	assert(value->type == FUNCTION_NAME);
	c = value->v.string_literal;
	name_length = strlen(value->v.string_literal);
	if ((2 < name_length) && ('$' == *c) && ('$' == *(c+1))) {
		// This is a mumps expression; leave it alone
		return 0;
	}
	new_name_value = &subs_array[3];
	// Check if this is a SQL standard function (e.g. ABS). If so return the corresponding function right here.
	// Note: Currently there is just ONE SQL standard function so it is hardcoded here. Once this list gets bigger,
	//       we might want to maintain the list of standard function names in a table and use a loop here to check.
	if ((LIT_LEN(ABS_FUNCTION_SQL_NAME) == name_length) && !MEMCMP_LIT(value->v.string_literal, ABS_FUNCTION_SQL_NAME))
	{
		alloc_len = LIT_LEN(ABS_FUNCTION_OCTO_NAME) + 1;	/* leave room for null terminator at end */
		new_name_value->buf_addr = octo_cmalloc(memory_chunks, alloc_len);
		MEMCPY_LIT(new_name_value->buf_addr, ABS_FUNCTION_OCTO_NAME);
		new_name_value->len_used = alloc_len - 1;
		new_name_value->len_alloc = alloc_len;
	} else
	{	// Now that we know this is not a SQL standard function, check if the user-defined function exists in the database
		// and if so fill in the translation into "value" (and in turn "stmt").
		octo_global = &subs_array[0];
		functions_sub = &subs_array[1];
		function_name_sub = &subs_array[2];

		YDB_STRING_TO_BUFFER(config->global_names.octo, octo_global);
		YDB_LITERAL_TO_BUFFER("functions", functions_sub);
		function_name_sub->buf_addr = value->v.string_literal;
		function_name_sub->len_alloc = function_name_sub->len_used = name_length;
		new_name_value->len_used = 31;	// Start with 32 bytes (set to 31 so we do +1 in do/while loop) and expand as needed
		do {
			alloc_len = new_name_value->len_used + 1;
			new_name_value->buf_addr = octo_cmalloc(memory_chunks, alloc_len);
			new_name_value->len_alloc = alloc_len - 1;	/* leave room for null terminator at end */
			status = ydb_get_s(octo_global, 2, functions_sub, new_name_value);
			assert((YDB_ERR_INVSTRLEN != status) || (new_name_value->len_alloc < new_name_value->len_used));
		} while (YDB_ERR_INVSTRLEN == status);
		if (YDB_ERR_GVUNDEF == status) {
			// Not found. Else issue an error.
			ERROR(CUSTOM_ERROR, "Unknown function: %s", value->v.string_literal);
			return 1;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
		new_name_value->len_alloc = alloc_len;
	}
	// Replace the pointer in the value with the new value
	assert(new_name_value->len_used < new_name_value->len_alloc);	// Ensure space is there for null terminator
	new_name_value->buf_addr[new_name_value->len_used] = '\0';
	value->v.string_literal = new_name_value->buf_addr;
	return result;
}
