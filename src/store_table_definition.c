/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "octo.h"

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, SUB_BUFFER) \
	{                                                    \
		if (YDB_OK != STATUS) {                      \
			YDB_FREE_BUFFER(SUB_BUFFER);         \
			return STATUS;                       \
		}                                            \
	}

/* Store the binary or text representation of the CREATE TABLE statement in one or more of the following nodes
 *	^%ydboctoschema(table_name,OCTOLIT_BINARY,0)
 *	^%ydboctoschema(table_name,OCTOLIT_BINARY,1)
 *	^%ydboctoschema(table_name,OCTOLIT_BINARY,2)
 *	...
 * OR:
 *	^%ydboctoschema(table_name,OCTOLIT_TEXT,0)
 *	^%ydboctoschema(table_name,OCTOLIT_TEXT,1)
 *	^%ydboctoschema(table_name,OCTOLIT_TEXT,2)
 *	...
 * Each node can store up to MAX_DEFINITION_FRAGMENT_SIZE bytes.
 * Returns
 *	YDB_OK on success.
 *	YDB_ERR_* on failure.
 * Note: The below logic is very similar to "src/store_binary_function_definition.c".
 */
int store_table_definition(ydb_buffer_t *table_name_buff, char *table_defn, int table_defn_length, boolean_t is_text) {
	ydb_buffer_t table_buffer, schema_global, table_name_buffers[3], *sub_buffer;
	int	     i, cur_length, status;

	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	table_buffer.len_alloc = MAX_DEFINITION_FRAGMENT_SIZE;
	table_name_buffers[0] = *table_name_buff;
	if (is_text) {
		YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &table_name_buffers[1]);
	} else {
		YDB_STRING_TO_BUFFER(OCTOLIT_BINARY, &table_name_buffers[1]);
	}
	sub_buffer = &table_name_buffers[2];
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(sub_buffer, MAX_DEFINITION_FRAGMENT_SIZE);
	i = 0;
	cur_length = 0;
	while (cur_length < table_defn_length) {
		sub_buffer->len_used = snprintf(sub_buffer->buf_addr, sub_buffer->len_alloc, "%d", i);
		table_buffer.buf_addr = &table_defn[cur_length];
		if (MAX_DEFINITION_FRAGMENT_SIZE < (table_defn_length - cur_length)) {
			table_buffer.len_used = MAX_DEFINITION_FRAGMENT_SIZE;
		} else {
			table_buffer.len_used = table_defn_length - cur_length;
		}
		status = ydb_set_s(&schema_global, 3, table_name_buffers, &table_buffer);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, sub_buffer);
		cur_length += MAX_DEFINITION_FRAGMENT_SIZE;
		i++;
	}
	if (is_text) {
		YDB_STRING_TO_BUFFER(OCTOLIT_TEXT_LENGTH, &table_name_buffers[1]);
	} else {
		YDB_STRING_TO_BUFFER(OCTOLIT_LENGTH, &table_name_buffers[1]);
	}
	// Use sub_buffer as a temporary buffer below
	sub_buffer->len_used = snprintf(sub_buffer->buf_addr, sub_buffer->len_alloc, "%d", table_defn_length);
	status = ydb_set_s(&schema_global, 2, table_name_buffers, sub_buffer);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, sub_buffer);
	YDB_FREE_BUFFER(sub_buffer);
	return YDB_OK;
}
