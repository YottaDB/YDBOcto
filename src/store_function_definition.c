/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "octo.h"

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS) \
	{                                        \
		if (YDB_OK != STATUS) {          \
			return STATUS;           \
		}                                \
	}

/* Store the binary or text representation of the CREATE FUNCTION statement in one or more of the following nodes
 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_BINARY,0)
 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_BINARY,1)
 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_BINARY,2)
 *	...
 * OR:
 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT,0)
 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT,1)
 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT,2)
 *	...
 * Each node can store up to MAX_DEFINITION_FRAGMENT_SIZE bytes.
 * Assumes "function_name_buffers" is an array of 5 "ydb_buffer_t" structures with the following initialized
 *	function_name_buffers[0] = OCTOLIT_FUNCTIONS
 *	function_name_buffers[1] = function_name
 *	function_name_buffers[2] = function_hash
 * This function initializes the following
 *	function_name_buffers[3] = OCTOLIT_BINARY (or OCTOLIT_TEXT)
 *	function_name_buffers[4] = 0, 1, 2, ... as needed
 * Returns
 *	YDB_OK on success.
 *	YDB_ERR_* on failure.
 * Note: The below logic is very similar to "src/store_binary_table_definition.c".
 */
int store_function_definition(ydb_buffer_t *function_name_buffers, char *function_defn, int function_defn_length,
			      boolean_t is_text) {
	ydb_buffer_t function_buffer, octo_global, *sub_buffer;
	int	     i, cur_length, status;
	char	     fragment_num_buff[INT64_TO_STRING_MAX];

	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	function_buffer.len_alloc = MAX_DEFINITION_FRAGMENT_SIZE;
	if (is_text) {
		YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &function_name_buffers[3]);
	} else {
		YDB_STRING_TO_BUFFER(OCTOLIT_BINARY, &function_name_buffers[3]);
	}
	sub_buffer = &function_name_buffers[4];
	OCTO_SET_BUFFER((*sub_buffer), fragment_num_buff);
	// sub_buffer->buf_addr = fragment_num_buff;
	// sub_buffer->len_alloc = sizeof(fragment_num_buff);
	i = 0;
	cur_length = 0;
	while (cur_length < function_defn_length) {
		sub_buffer->len_used = snprintf(sub_buffer->buf_addr, sub_buffer->len_alloc, "%d", i);
		function_buffer.buf_addr = &function_defn[cur_length];
		if (MAX_DEFINITION_FRAGMENT_SIZE < (function_defn_length - cur_length)) {
			function_buffer.len_used = MAX_DEFINITION_FRAGMENT_SIZE;
		} else {
			function_buffer.len_used = function_defn_length - cur_length;
		}
		status = ydb_set_s(&octo_global, 5, function_name_buffers, &function_buffer);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status);
		cur_length += MAX_DEFINITION_FRAGMENT_SIZE;
		i++;
	}
	if (is_text) {
		YDB_STRING_TO_BUFFER(OCTOLIT_TEXT_LENGTH, &function_name_buffers[3]);
	} else {
		YDB_STRING_TO_BUFFER(OCTOLIT_LENGTH, &function_name_buffers[3]);
	}
	// Use sub_buffer as a temporary buffer below
	sub_buffer->len_used = snprintf(sub_buffer->buf_addr, sub_buffer->len_alloc, "%d", function_defn_length);
	status = ydb_set_s(&octo_global, 4, function_name_buffers, sub_buffer);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status);
	return YDB_OK;
}
