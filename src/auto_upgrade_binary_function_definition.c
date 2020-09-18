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
		if (NULL != RET_BUFF.buf_addr) {       \
			YDB_FREE_BUFFER(&RET_BUFF);    \
		}                                      \
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

	ret_buff.buf_addr = NULL; // Allow macros to verify if cleanup is needed and prevent clang compiler warnings
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	/* $order through ^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name) and for each function_name,
	 * $order through ^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash) and for each function_hash,
	 * get CREATE FUNCTION statement from
	 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT)
	 * OR
	 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT,...)
	 * AND set the following nodes
	 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_BINARY,...)
	 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_LENGTH)
	 */
	YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_subs[0]);
	YDB_MALLOC_BUFFER(&function_subs[1], YDB_MAX_KEY_SZ); /* to store the function name */
	YDB_MALLOC_BUFFER(&function_subs[2], YDB_MAX_KEY_SZ); /* to store the function hash */
	function_subs[1].len_used = 0;
	function_buff = &function_subs[0]; /* Note down that this buffer needs to be freed in case of error code path */
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
			char	     text_defn_str[MAX_DEFINITION_FRAGMENT_SIZE];
			long	     text_defn_len;
			ydb_buffer_t text_defn_buff;
			ydb_buffer_t cursor_ydb_buff;
			ydb_long_t   cursorId;
			char	     cursor_buffer[INT64_TO_STRING_MAX];
			char	     cur_frag_str[INT64_TO_STRING_MAX];
			SqlFunction *function;
			SqlValue *   value;
			long long    function_oid;
			char *	     as_with_spaces = " AS ", *curstr, *prevstr;
			size_t	     as_len;
			unsigned int data_ret;

			status = ydb_subscript_next_s(&octo_global, 3, &function_subs[0], &function_subs[2]);
			if (YDB_ERR_NODEEND == status) {
				break;
			}
			assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);

			YDB_STRING_TO_BUFFER(OCTOLIT_TEXT_LENGTH, &function_subs[3]);
			status = ydb_data_s(&octo_global, 4, &function_subs[0], &data_ret);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
			/* We expect a subtree except for older commits that pre-date text definition fragmentation, so check for
			 * for the presence of OCTOLIT_TEXT_LENGTH, as this node is only created during text definition
			 * fragmentation. Hence, it will not be present on older commits. In that case, we expect an absent node,
			 * i.e. ydb_data_s returns 0 (no node or subtree).
			 */
			if (0 == data_ret) {
				/* Retrieve text definition using pre-fragmentation layout:
				 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_TEXT)
				 */
				YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &function_subs[3]);
				YDB_MALLOC_BUFFER(&ret_buff, OCTO_INIT_BUFFER_LEN);
				status = ydb_get_s(&octo_global, 4, &function_subs[0], &ret_buff);
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(ret_buff);
					status = ydb_get_s(&octo_global, 4, &function_subs[0], &ret_buff);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				/* The function definition in question doesn't exist in the old or new format, and so cannot be
				 * automatically upgraded.
				 */
				if (YDB_OK != status) {
					ERROR(ERR_AUTO_UPGRADE, "");
				}
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
			} else {
				// Get the length of the full text table definition
				OCTO_SET_BUFFER(text_defn_buff, text_defn_str);
				OCTO_SET_BUFFER(function_subs[4], cur_frag_str);
				YDB_STRING_TO_BUFFER(OCTOLIT_TEXT_LENGTH, &function_subs[3]);
				status = ydb_get_s(&octo_global, 4, &function_subs[0], &text_defn_buff);
				/* Assert since we allocated MAX_DEFINITION_FRAGMENT_SIZE above and
				 * MAX_DEFINITION_FRAGMENT_SIZE > INT64_TO_STRING_MAX (INT64_TO_STRING_MAX is the size of the buffer
				 * originally stored in store_binary_function_definition.c)
				 */
				assert(YDB_ERR_INVSTRLEN != status);
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);

				text_defn_len = strtoll(text_defn_buff.buf_addr, NULL, 10);
				if ((LLONG_MIN == text_defn_len) || (LLONG_MAX == text_defn_len)) {
					ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), text_defn_buff.buf_addr);
					CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE);
				}
				YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &function_subs[3]); // Reset subscript
				YDB_MALLOC_BUFFER(&ret_buff, text_defn_len + 1);       /* to store the return */
				ret_buff.len_used = 0;
				// Retrieve each text definition fragment containing up to MAX_DEFINITION_FRAGMENT_SIZE characters
				// each
				do {
					status = ydb_subscript_next_s(&octo_global, 5, &function_subs[0], &function_subs[4]);
					assert(YDB_ERR_INVSTRLEN != status); // Since MAX_DEFINITION_FRAGMENT_SIZE allocated above
					function_subs[4].buf_addr[function_subs[4].len_used] = '\0';
					if (YDB_ERR_NODEEND == status) {
						break;
					}
					CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
					status = ydb_get_s(&octo_global, 5, &function_subs[0], &text_defn_buff);
					assert(YDB_ERR_INVSTRLEN
					       != status); /* because we allocated MAX_DEFINITION_FRAGMENT_SIZE above */
					CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE);
					/* Because we allocated text_defn_len above. More than this shouldn't have been stored by
					 * store_function_definition.
					 */
					assert(ret_buff.len_alloc >= (ret_buff.len_used + text_defn_buff.len_used + 1));
					memcpy(&ret_buff.buf_addr[ret_buff.len_used], text_defn_buff.buf_addr,
					       text_defn_buff.len_used);
					ret_buff.len_used += text_defn_buff.len_used;
				} while (ret_buff.len_used <= text_defn_len);
			}

			/* Check if back-quotes surround the M extrinsic (a bug that is fixed in later commits). If so remove it.
			 * Find last occurrence of " AS " in string first.
			 */
			ret_buff.buf_addr[ret_buff.len_used] = '\0';
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
			status = store_function_definition(&function_subs[0], binary_function_defn, binary_function_defn_length,
							   FALSE);
			free(binary_function_defn); /* free buffer that was "malloc"ed in "compress_statement" */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, TRUE);
		}
	}
	CLEANUP_FUNCTION_BUFF(function_buff, ret_buff);
	return YDB_OK;
}
