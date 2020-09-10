/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "octo.h"

#define CLEANUP_FUNCTION_BUFF(FUNCTION_BUFF, RET_BUFF) \
	{                                              \
		YDB_FREE_BUFFER(&FUNCTION_BUFF[1]);    \
		YDB_FREE_BUFFER(&FUNCTION_BUFF[2]);    \
		YDB_FREE_BUFFER(&RET_BUFF);            \
	}

#define CLEANUP_AND_RETURN(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK) \
	{                                                                      \
		CLEANUP_FUNCTION_BUFF(FUNCTION_BUFF, RET_BUFF);                \
		if (FREE_MEMORY_CHUNK) {                                       \
			OCTO_CFREE(memory_chunks);                             \
		}                                                              \
		return STATUS;                                                 \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK)    \
	{                                                                                       \
		if (YDB_OK != STATUS) {                                                         \
			YDB_ERROR_CHECK(STATUS);                                                \
			CLEANUP_AND_RETURN(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK); \
		}                                                                               \
	}

/* Automatically upgrade all binary function definitions.
 * Returns YDB_OK on success and a non-zero (YDB_ERR_* status code or 1) on errors.
 * Note: The below logic is similar to that in "src/auto_upgrade_binary_table_definition.c".
 */
int auto_upgrade_binary_function_definition(void) {
	ydb_buffer_t  octo_global, schema_global, function_subs[5], *function_buff, ret_buff;
	int	      status;
	SqlStatement *result;
	ParseContext  parse_context;

	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	/* $order through ^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name) and for each function_name,
	 * $order through ^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash) and for each function_hash,
	 * get CREATE FUNCTION statement from
	 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT)
	 * and set the following nodes
	 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_BINARY,...)
	 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_LENGTH)
	 */
	YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_subs[0]);
	YDB_MALLOC_BUFFER(&function_subs[1], YDB_MAX_KEY_SZ); /* to store the function name */
	YDB_MALLOC_BUFFER(&function_subs[2], YDB_MAX_KEY_SZ); /* to store the function hash */
	function_subs[1].len_used = 0;
	YDB_MALLOC_BUFFER(&ret_buff, YDB_MAX_STR); /* to store the return */
	function_buff = &function_subs[0];	   /* Note down that this buffer needs to be freed in case of error code path */
	while (TRUE) {
		status = ydb_subscript_next_s(&octo_global, 2, &function_subs[0], &function_subs[1]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
		/* A given function name could have multiple function definitions each with a different hash.
		 * Loop through each of them.
		 */
		function_subs[2].len_used = 0;
		while (TRUE) {
			char *	     binary_function_defn; /* pointer to the binary function definition */
			int	     binary_function_defn_length;
			ydb_buffer_t cursor_ydb_buff;
			ydb_long_t   cursorId;
			char	     cursor_buffer[INT64_TO_STRING_MAX];
			SqlFunction *function;
			SqlValue *   value;
			long long    function_oid;
			char *	     as_with_spaces = " AS ", *curstr, *prevstr;
			size_t	     as_len;

			status = ydb_subscript_next_s(&octo_global, 3, &function_subs[0], &function_subs[2]);
			if (YDB_ERR_NODEEND == status) {
				break;
			}
			assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
			/* Get the "CREATE FUNCTION" query corresponding to "function_subs[1]" (function name)
			 * and "function_subs[2]" (function hash) and recompute binary definition.
			 */
			YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &function_subs[3]);
			status = ydb_get_s(&octo_global, 4, &function_subs[0], &ret_buff);
			assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_STR above */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
			assert(ret_buff.len_used < ret_buff.len_alloc);
			ret_buff.buf_addr[ret_buff.len_used] = '\0'; /* null terminate so we can use "strstr()" */
			/* Check if back-quotes surround the M extrinsic (a bug that is fixed in later commits). If so remove it.
			 * Find last occurrence of " AS " in string first.
			 */
			curstr = ret_buff.buf_addr;
			as_len = strlen(as_with_spaces);
			for (;;) {
				prevstr = curstr;
				curstr = strstr(curstr, as_with_spaces);
				if (NULL == curstr) {
					assert(prevstr != ret_buff.buf_addr); /* there should be at least one " AS " */
					break;
				} else {
					curstr = curstr + as_len;
				}
			}
			if (prevstr != ret_buff.buf_addr) {
				size_t len;
				char * endstr;

				/* Found " AS ". Check if back-quotes surround M extrinsic that follows this. If so remove them. */
				curstr = prevstr;
				len = strlen(curstr);
				if (2 < len) {
					endstr = curstr + len - 2;
					if (('`' == *curstr) && (';' == endstr[1]) && ('`' == *endstr)) {
						memmove(curstr, curstr + 1, endstr - curstr - 1);
						curstr = endstr - 1;
						*curstr++ = ';';
						*curstr++ = '\0';
						ret_buff.len_used -= 2;
						/* Store this fixed "text" node back into the database so we don't need
						 * to convert again.
						 */
						status = ydb_set_s(&octo_global, 4, &function_subs[0], &ret_buff);
						CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
					}
				}
			}

			COPY_QUERY_TO_INPUT_BUFFER(ret_buff.buf_addr, (int)ret_buff.len_used, NEWLINE_NEEDED_FALSE);
			/* Note: Following code is similar to that in octo.c and run_query.c */
			memset(&parse_context, 0, sizeof(parse_context));
			memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
			/* From now on, all CLEANUP_* macro calls will have TRUE as last parameter
			 * to indicate "OCTO_CFREE(memory_chunks)" is needed for proper cleanup.
			 */
			cursor_ydb_buff.buf_addr = cursor_buffer;
			cursor_ydb_buff.len_alloc = sizeof(cursor_buffer);
			cursorId = create_cursor(&schema_global, &cursor_ydb_buff);
			if (0 > cursorId) {
				CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE);
			}
			parse_context.cursorId = cursorId;
			parse_context.cursorIdString = cursor_ydb_buff.buf_addr;
			/* To print only the current query store the index for the last one
			 * then print the difference between the cur_input_index - old_input_index
			 */
			old_input_index = cur_input_index;
			result = parse_line(&parse_context);
#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
			INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
#endif
			if (NULL == result) {
				INFO(INFO_RETURNING_FAILURE, "parse_line");
				CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE);
			}
			/* Get OID of the function_name and function_hash combination (from below gvn) as we need that OID
			 * to store in the binary function definition.
			 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_OID)=FUNCTIONOID
			 */
			YDB_STRING_TO_BUFFER(OCTOLIT_OID, &function_subs[3]);
			status = ydb_get_s(&octo_global, 4, &function_subs[0], &ret_buff);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, TRUE);
			assert(ret_buff.len_used < ret_buff.len_alloc);
			ret_buff.buf_addr[ret_buff.len_used] = '\0'; /* null terminate for "strtoll" */
			function_oid = strtoll(ret_buff.buf_addr, NULL, 10);
			if ((LLONG_MIN == function_oid) || (LLONG_MAX == function_oid)) {
				ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), ret_buff.buf_addr);
				CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE);
			}
			assert(create_function_STATEMENT == result->type);
			UNPACK_SQL_STATEMENT(function, result, create_function);
			/* Store OID in the SqlFunction structure so it goes in the binary function definition
			 * as part of the "compress_statement" call done below.
			 */
			function->oid = function_oid;

			/* Add function hash to parse tree so it also gets stored in binary function definition */
			SQL_STATEMENT(function->function_hash, value_STATEMENT);
			MALLOC_STATEMENT(function->function_hash, value, SqlValue);
			UNPACK_SQL_STATEMENT(value, function->function_hash, value);
			value->v.string_literal = octo_cmalloc(memory_chunks, function_subs[2].len_used + 1);
			memcpy(value->v.string_literal, function_subs[2].buf_addr, function_subs[2].len_used);
			value->v.string_literal[function_subs[2].len_used] = '\0'; /* null terminate */
			value->type = FUNCTION_HASH;

			binary_function_defn = NULL;
			compress_statement(result, &binary_function_defn,
					   &binary_function_defn_length); /* sets "binary_function_defn" to "malloc"ed storage */
			assert(NULL != binary_function_defn);
			/* Note: function_subs[4] initialized and used in the function call below */
			status = store_binary_function_definition(&function_subs[0], binary_function_defn,
								  binary_function_defn_length);
			free(binary_function_defn); /* free buffer that was "malloc"ed in "compress_statement" */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, TRUE);
		}
	}
	CLEANUP_FUNCTION_BUFF(function_buff, ret_buff);
	return YDB_OK;
}
