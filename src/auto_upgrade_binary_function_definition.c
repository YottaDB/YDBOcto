/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
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

#define CLEANUP_AND_RETURN(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF) \
	{                                                                                       \
		CLEANUP_FUNCTION_BUFF(FUNCTION_BUFF, RET_BUFF);                                 \
		if (FREE_MEMORY_CHUNK) {                                                        \
			OCTO_CFREE(memory_chunks);                                              \
		}                                                                               \
		if (NULL != CURSOR_YDB_BUFF) {                                                  \
			DELETE_QUERY_PARAMETER_CURSOR_LVN(CURSOR_YDB_BUFF);                     \
		}                                                                               \
		return STATUS;                                                                  \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF)    \
	{                                                                                                        \
		if (YDB_OK != STATUS) {                                                                          \
			YDB_ERROR_CHECK(STATUS);                                                                 \
			CLEANUP_AND_RETURN(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF); \
		}                                                                                                \
	}

#define CLEANUP_DANGLING_FUNCTION_NODES_AND_RETURN_IF_NEEDED(SUBLIT, OCTO_GLOBAL, FUNCTION_SUBS, DATA_RET, STATUS, FUNCTION_BUFF, \
							     RET_BUFF, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF)                        \
	{                                                                                                                         \
		YDB_STRING_TO_BUFFER(SUBLIT, &FUNCTION_SUBS[3]);                                                                  \
		STATUS = ydb_data_s(&OCTO_GLOBAL, 4, &FUNCTION_SUBS[0], &DATA_RET);                                               \
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF);            \
		if (0 == DATA_RET) {                                                                                              \
			STATUS = ydb_delete_s(&OCTO_GLOBAL, 3, &FUNCTION_SUBS[0], YDB_DEL_TREE);                                  \
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, FUNCTION_BUFF, RET_BUFF, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF);    \
			CLEANUP_FUNCTION_BUFF(FUNCTION_BUFF, RET_BUFF);                                                           \
			return YDB_OK;                                                                                            \
		}                                                                                                                 \
	}

/* Automatically upgrade all binary function definitions.
 * Returns YDB_OK on success and a non-zero (YDB_ERR_* status code or 1) on errors.
 * Note: The below logic is similar to that in "src/auto_upgrade_binary_table_definition.c".
 */
int auto_upgrade_binary_function_definition(void) {
	ydb_buffer_t  octo_global, function_subs[5], *function_buff, ret_buff;
	int	      status;
	SqlStatement *result;
	ParseContext  parse_context;

	ret_buff.buf_addr = NULL; // Allow macros to verify if cleanup is needed and prevent clang compiler warnings
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
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
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&function_subs[1], YDB_MAX_KEY_SZ); /* to store the function name */
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&function_subs[2], YDB_MAX_KEY_SZ); /* to store the function hash */
	function_subs[1].len_used = 0;
	function_buff = &function_subs[0]; /* Note down that this buffer needs to be freed in case of error code path */
	while (TRUE) {
		status = ydb_subscript_next_s(&octo_global, 2, &function_subs[0], &function_subs[1]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);
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
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);

			YDB_STRING_TO_BUFFER(OCTOLIT_TEXT_LENGTH, &function_subs[3]);
			status = ydb_data_s(&octo_global, 4, &function_subs[0], &data_ret);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);
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
				OCTO_MALLOC_NULL_TERMINATED_BUFFER(&ret_buff, OCTO_INIT_BUFFER_LEN);
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
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);
			} else {
				/* Check for missing function definitions nodes (OCTOLIT_LENGTH and OCTOLIT_OID) that will be absent
				 * for commits prior or equal to eeae6bc7 due to a bug that allowed the retention of partially
				 * populated function definition trees in the case of an ERR_TOO_MANY_FUNCTION_ARGUMENTS error. In
				 * that case, cleanup the erroneously retained nodes and continue without attempting to reload the
				 * function definition.
				 */
				CLEANUP_DANGLING_FUNCTION_NODES_AND_RETURN_IF_NEEDED(OCTOLIT_LENGTH, octo_global, function_subs,
										     data_ret, status, function_buff, ret_buff,
										     FALSE, NULL);
				CLEANUP_DANGLING_FUNCTION_NODES_AND_RETURN_IF_NEEDED(OCTOLIT_OID, octo_global, function_subs,
										     data_ret, status, function_buff, ret_buff,
										     FALSE, NULL);

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
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);

				// Null terminate for strtoll
				text_defn_buff.buf_addr[text_defn_buff.len_used] = '\0';
				text_defn_len = strtoll(text_defn_buff.buf_addr, NULL, 10);

				if ((LLONG_MIN == text_defn_len) || (LLONG_MAX == text_defn_len)) {
					ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), text_defn_buff.buf_addr);
					CLEANUP_AND_RETURN(1, function_buff, ret_buff, FALSE, NULL);
				}
				YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &function_subs[3]);	      // Reset subscript
				OCTO_MALLOC_NULL_TERMINATED_BUFFER(&ret_buff, text_defn_len); /* to store the return */
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
					CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);
					status = ydb_get_s(&octo_global, 5, &function_subs[0], &text_defn_buff);
					assert(YDB_ERR_INVSTRLEN
					       != status); /* because we allocated MAX_DEFINITION_FRAGMENT_SIZE above */
					CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);
					/* Because we allocated text_defn_len above. More than this shouldn't have been stored by
					 * store_function_definition.
					 */
					assert(ret_buff.len_alloc >= (ret_buff.len_used + text_defn_buff.len_used));
					memcpy(&ret_buff.buf_addr[ret_buff.len_used], text_defn_buff.buf_addr,
					       text_defn_buff.len_used);
					ret_buff.len_used += text_defn_buff.len_used;
				} while (ret_buff.len_used <= text_defn_len);
			}

			/* Check if back-quotes surround the M extrinsic (a bug that is fixed in later commits). If so remove it.
			 * Find last occurrence of " AS " in string first.
			 */
			ret_buff.buf_addr[ret_buff.len_used]
			    = '\0'; /* Space for null terminator allocated by OCTO_MALLOC_NULL_TERMINATED_BUFFER and/or
				       EXPAND_YDB_BUFFER_T_ALLOCATION calls above */
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
						CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, FALSE, NULL);
					}
				}
			}
			COPY_QUERY_TO_INPUT_BUFFER(ret_buff.buf_addr, (int)ret_buff.len_used, NEWLINE_NEEDED_FALSE);
			/* Note: Following code is similar to that in octo.c and run_query.c */
			memset(&parse_context, 0, sizeof(parse_context));
			memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
			/* From now on, all CLEANUP_* macro calls will have TRUE as the last-but-one parameter
			 * to indicate "OCTO_CFREE(memory_chunks)" is needed for proper cleanup.
			 */
			cursor_ydb_buff.buf_addr = cursor_buffer;
			cursor_ydb_buff.len_alloc = sizeof(cursor_buffer);
			cursorId = create_cursor(&octo_global, &cursor_ydb_buff);
			if (0 > cursorId) {
				CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE, NULL);
			}
			parse_context.cursorId = cursorId;
			parse_context.cursorIdString = cursor_ydb_buff.buf_addr;
			/* To print only the current query store the index for the last one
			 * then print the difference between the cur_input_index - old_input_index
			 */
			assert(0 == cur_input_index);
			old_input_index = cur_input_index;
			result = parse_line(&parse_context);
			/* From now on, all CLEANUP_* macro calls will have TRUE as the last parameter
			 * to indicate "DELETE_QUERY_PARAMETER_CURSOR_LVN " is needed to cleanup/delete any query parameter
			 * related lvn nodes and avoid lvn buildup across multiple such invocations of "parse_line()"
			 * in this "for" loop.
			 */
#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
			INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
#endif
			if (NULL == result) {
				INFO(INFO_RETURNING_FAILURE, "parse_line");
				CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE, &cursor_ydb_buff);
			}
			if (config->is_auto_upgrade_octo929) {
				hash128_state_t state;
				char		function_hash[MAX_ROUTINE_LEN + 1];
				int		status;
				INVOKE_HASH_CANONICAL_QUERY(state, result, status); /* "state" holds final hash */
				if (0 != status) {
					CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE, &cursor_ydb_buff);
				}
				generate_name_type(FunctionHash, &state, 0, function_hash, sizeof(function_hash));

				ydb_buffer_t ydbocto929, func_subs[2];
				char	     subs0_buff[INT32_TO_STRING_MAX];
				unsigned int data_ret;

				YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTO929, &ydbocto929);
				func_subs[0].buf_addr = subs0_buff;
				func_subs[0].len_alloc = sizeof(subs0_buff);
				func_subs[0].len_used
				    = snprintf(func_subs[0].buf_addr, func_subs[0].len_alloc, "%d", create_function_STATEMENT);
				YDB_STRING_TO_BUFFER(function_hash, &func_subs[1]);
				status = ydb_data_s(&ydbocto929, 2, &func_subs[0], &data_ret);
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, TRUE, &cursor_ydb_buff);
				if (0 == data_ret) {
					fprintf(config->octo929_sqlfile_stream, "%.*s\n", cur_input_index - old_input_index,
						input_buffer_combined + old_input_index);
				}
			}
			/* Get OID of the function_name and function_hash combination (from below gvn) as we need that OID
			 * to store in the binary function definition.
			 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,function_hash,OCTOLIT_OID)=FUNCTIONOID
			 */
			YDB_STRING_TO_BUFFER(OCTOLIT_OID, &function_subs[3]);
			status = ydb_get_s(&octo_global, 4, &function_subs[0], &ret_buff);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, TRUE, &cursor_ydb_buff);
			assert(ret_buff.len_used < ret_buff.len_alloc);
			ret_buff.buf_addr[ret_buff.len_used] = '\0'; /* null terminate for "strtoll" */
			function_oid = strtoll(ret_buff.buf_addr, NULL, 10);
			if ((LLONG_MIN == function_oid) || (LLONG_MAX == function_oid)) {
				ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), ret_buff.buf_addr);
				CLEANUP_AND_RETURN(1, function_buff, ret_buff, TRUE, &cursor_ydb_buff);
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
			compress_statement(result, &binary_function_defn, &binary_function_defn_length,
					   FALSE); /* sets "binary_function_defn" to "malloc"ed storage */
			assert(NULL != binary_function_defn);
			/* Note: function_subs[4] initialized and used in the function call below */
			status = store_function_definition(&function_subs[0], binary_function_defn, binary_function_defn_length,
							   FALSE);
			free(binary_function_defn); /* free buffer that was "malloc"ed in "compress_statement" */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, function_buff, ret_buff, TRUE, &cursor_ydb_buff);
			/* Cleanup any memory allocations in "parse_line()" of this iteration before moving on to next iteration
			 * to avoid memory buildup that can happen if we have to process thousands of binary definitions.
			 * Also free memory used in this iteration for ret_buff
			 */
			OCTO_CFREE(memory_chunks);
			DELETE_QUERY_PARAMETER_CURSOR_LVN(&cursor_ydb_buff);
			YDB_FREE_BUFFER(&ret_buff);
		}
	}
	ERASE_INPUT_BUFFER; /* Clear history of all "parse_line()" processing related to binary function upgrade to avoid
			     * confusion when we next proceed to run real queries.
			     */
	CLEANUP_FUNCTION_BUFF(function_buff, ret_buff);
	return YDB_OK;
}
