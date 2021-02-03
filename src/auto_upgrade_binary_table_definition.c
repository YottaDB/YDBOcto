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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "octo.h"

#define CLEANUP_TABLE_BUFF(TABLE_BUFF)           \
	{                                        \
		YDB_FREE_BUFFER(&TABLE_BUFF[0]); \
		YDB_FREE_BUFFER(&TABLE_BUFF[2]); \
	}

#define CLEANUP_AND_RETURN(STATUS, TABLE_BUFF, TEXT_DEFN, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF) \
	{                                                                                     \
		if (NULL != TABLE_BUFF) {                                                     \
			CLEANUP_TABLE_BUFF(TABLE_BUFF);                                       \
		}                                                                             \
		if (NULL != TEXT_DEFN) {                                                      \
			free(TEXT_DEFN);                                                      \
		}                                                                             \
		if (FREE_MEMORY_CHUNK) {                                                      \
			OCTO_CFREE(memory_chunks);                                            \
		}                                                                             \
		if (NULL != CURSOR_YDB_BUFF) {                                                \
			DELETE_QUERY_PARAMETER_CURSOR_LVN(CURSOR_YDB_BUFF);                   \
		}                                                                             \
		return STATUS;                                                                \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, TABLE_BUFF, TEXT_DEFN, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF)    \
	{                                                                                                      \
		if (YDB_OK != STATUS) {                                                                        \
			YDB_ERROR_CHECK(STATUS);                                                               \
			CLEANUP_AND_RETURN(STATUS, TABLE_BUFF, TEXT_DEFN, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF); \
		}                                                                                              \
	}

/* Automatically upgrade all binary table definitions.
 * Returns YDB_OK on success and a non-zero (YDB_ERR_* status code or 1) on errors.
 * Note: The below logic is similar to that in "src/auto_upgrade_binary_function_definition.c".
 */
int auto_upgrade_binary_table_definition(void) {
	ydb_buffer_t  octo_global, schema_global, table_subs[3], *table_buff;
	int	      status;
	SqlStatement *result;
	ParseContext  parse_context;

	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	/* $order through ^%ydboctoschema(table_name) and for each table_name, get CREATE TABLE statement from
	 *	^%ydboctoschema(table_name,OCTOLIT_TEXT)
	 * OR
	 *	^%ydboctoschema(table_name,OCTOLIT_TEXT,0)
	 *	^%ydboctoschema(table_name,OCTOLIT_TEXT,1)
	 * AND set the following nodes
	 *	^%ydboctoschema(table_name,OCTOLIT_BINARY,...)
	 *	^%ydboctoschema(table_name,OCTOLIT_LENGTH).
	 * Note: We need to account for multiple node configurations for text nodes as the changes for YDBOcto#590 introduced text
	 * definition fragmentation to allow for arbitrarily long DDL definitions. However, earlier versions of Octo do not have
	 * this layout, and so we must handle both cases for backward compatibility.
	 */
	YDB_MALLOC_BUFFER(&table_subs[0], YDB_MAX_KEY_SZ); /* to store the table name */
	table_subs[0].len_used = 0;
	YDB_MALLOC_BUFFER(&table_subs[2], MAX_DEFINITION_FRAGMENT_SIZE); /* to store the return */
	table_buff = &table_subs[0]; /* Note down that this buffer needs to be freed in case of error code path */
	while (TRUE) {
		char *	     binary_table_defn; /* pointer to the binary table definition */
		int	     binary_table_defn_length;
		ydb_buffer_t cursor_ydb_buff;
		ydb_long_t   cursorId;
		char	     cursor_buffer[INT64_TO_STRING_MAX];
		long long    table_oid;
		unsigned int data_ret;
		SqlTable *   table;

		status = ydb_subscript_next_s(&schema_global, 1, &table_subs[0], &table_subs[0]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);
		/* Get the "CREATE TABLE" query corresponding to "table_buff" and recompute binary definition.
		 *	^%ydboctoschema(table_name,OCTOLIT_TEXT)
		 */
		YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &table_subs[1]);
		status = ydb_data_s(&schema_global, 2, &table_subs[0], &data_ret);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);
		/* We expect a subtree except for older commits that pre-date text definition fragmentation, so check for value-only
		 * and absent nodes, i.e. ydb_data_s returns 0 (no node or subtree) or 1 (value but no subtree).
		 */
		if (1 >= data_ret) {
			/* For some prior Octo commits, the text definition was stored in a subscript "t" (instead of "text").
			 * So check that too. Note that even if a definition is stored in this manner, it cannot safely be used to
			 * auto-upgrade since it indicates a pre-r1.0.0 version of Octo and the auto-upgrade process is
			 * backward-incompatible with pre-r1.0.0 versions of Octo.
			 */
			if (0 == data_ret) {
				YDB_STRING_TO_BUFFER(OCTOLIT_T, &table_subs[1]);
			}
			status = ydb_get_s(&schema_global, 2, &table_subs[0], &table_subs[2]);
			// Expand buffer if value between 32KiB (current buffer initial size) and 1MiB (buffer size in prior
			// commits)
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(table_subs[2]);
				status = ydb_get_s(&schema_global, 2, &table_subs[0], &table_subs[2]);
				assert(YDB_ERR_INVSTRLEN != status);
			}
			if ((0 == data_ret) && (YDB_OK == status)) {
				/* There are other issues because of which auto upgrade is not possible.
				 * For example, the DDL had "$C(1)" in the DELIM (table level) for the CREATE TABLE.
				 * It is not considered necessary to try and auto upgrade such text definitions for now.
				 * So issue an error that asks the user to run a manual upgrade.
				 */
				ERROR(ERR_AUTO_UPGRADE, "");
				CLEANUP_AND_RETURN(1, table_buff, NULL, FALSE, NULL);
			}
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);
			COPY_QUERY_TO_INPUT_BUFFER(table_subs[2].buf_addr, (int)table_subs[2].len_used, NEWLINE_NEEDED_FALSE);
		} else {
			ydb_buffer_t text_defn_buff;
			char	     text_defn_str[MAX_DEFINITION_FRAGMENT_SIZE];
			char *	     text_defn = NULL;
			long	     text_defn_len, cur_len;

			text_defn_buff.buf_addr = text_defn_str;
			text_defn_buff.len_alloc = sizeof(text_defn_str);
			text_defn_buff.len_used = 0;
			// Get the length of the full text table definition
			YDB_STRING_TO_BUFFER(OCTOLIT_TEXT_LENGTH, &table_subs[1]);
			status = ydb_get_s(&schema_global, 2, &table_subs[0], &text_defn_buff);
			assert(YDB_ERR_INVSTRLEN != status); /* because we allocated MAX_DEFINITION_FRAGMENT_SIZE above */
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);

			text_defn_len = strtoll(text_defn_buff.buf_addr, NULL, 10);
			if ((LLONG_MIN == text_defn_len) || (LLONG_MAX == text_defn_len)) {
				ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), text_defn_buff.buf_addr);
				CLEANUP_AND_RETURN(1, table_buff, NULL, FALSE, NULL);
			}
			YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &table_subs[1]); // Reset subscript
			cur_len = 0;
			text_defn = (char *)malloc(sizeof(char) * text_defn_len);
			table_subs[2].len_used = 0; // Reset subscript for ydb_subscript_next_s
			do {
				status = ydb_subscript_next_s(&schema_global, 3, &table_subs[0], &table_subs[2]);
				table_subs[2].buf_addr[table_subs[2].len_used] = '\0';
				if (YDB_ERR_NODEEND == status) {
					status = YDB_OK;
					break;
				}
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, text_defn, FALSE, NULL);
				status = ydb_get_s(&schema_global, 3, &table_subs[0], &text_defn_buff);
				assert(YDB_ERR_INVSTRLEN != status); /* because we allocated MAX_DEFINITION_FRAGMENT_SIZE above */
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, text_defn, FALSE, NULL);
				memcpy(&text_defn[cur_len], text_defn_buff.buf_addr, text_defn_buff.len_used);
				cur_len += text_defn_buff.len_used;
			} while (cur_len <= text_defn_len);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, text_defn, FALSE, NULL);
			COPY_QUERY_TO_INPUT_BUFFER(text_defn, text_defn_len, NEWLINE_NEEDED_FALSE);
			free(text_defn);
		}

		/* Note: Following code is similar to that in octo.c and run_query.c */
		memset(&parse_context, 0, sizeof(parse_context));
		memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
		/* From now on, all CLEANUP_* macro calls will have TRUE as the last-but-one parameter
		 * to indicate "OCTO_CFREE(memory_chunks)" is needed for proper cleanup.
		 */
		cursor_ydb_buff.buf_addr = cursor_buffer;
		cursor_ydb_buff.len_alloc = sizeof(cursor_buffer);
		cursorId = create_cursor(&schema_global, &cursor_ydb_buff);
		if (0 > cursorId) {
			CLEANUP_AND_RETURN(1, table_buff, NULL, TRUE, NULL);
		}
		parse_context.cursorId = cursorId;
		parse_context.cursorIdString = cursor_ydb_buff.buf_addr;

		/* To print only the current query store the index for the last one
		 * then print the difference between the cur_input_index - old_input_index
		 */
		old_input_index = cur_input_index;
		result = parse_line(&parse_context);
		/* From now on, all CLEANUP_* macro calls will have TRUE as the last parameter to indicate
		 * "DELETE_QUERY_PARAMETER_CURSOR_LVN " is needed to cleanup/delete any query parameter
		 * related lvn nodes and avoid lvn buildup across multiple such invocations of "parse_line()"
		 * in this "for" loop.
		 */
#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
		INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
#endif
		if (NULL == result) {
			INFO(INFO_RETURNING_FAILURE, "parse_line");
			CLEANUP_AND_RETURN(1, table_buff, NULL, TRUE, &cursor_ydb_buff);
		}
		/* Get OID of the table name (from below gvn) as we need that OID to store in the binary table definition.
		 *	^%ydboctoschema(table_name,OCTOLIT_PG_CLASS)=TABLEOID
		 */
		YDB_STRING_TO_BUFFER(OCTOLIT_PG_CLASS, &table_subs[1]);
		status = ydb_get_s(&schema_global, 2, &table_subs[0], &table_subs[2]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, TRUE, &cursor_ydb_buff);
		assert(table_subs[2].len_used < table_subs[2].len_alloc);
		table_subs[2].buf_addr[table_subs[2].len_used] = '\0'; /* null terminate for "strtoll" */
		table_oid = strtoll(table_subs[2].buf_addr, NULL, 10);
		if ((LLONG_MIN == table_oid) || (LLONG_MAX == table_oid)) {
			ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), table_subs[2].buf_addr);
			CLEANUP_AND_RETURN(1, table_buff, NULL, TRUE, &cursor_ydb_buff);
		}
		assert(create_table_STATEMENT == result->type);
		UNPACK_SQL_STATEMENT(table, result, create_table);
		/* Store OID in the SqlTable structure so it goes in the binary table definition as part of "compress_statement" */
		table->oid = table_oid;

		binary_table_defn = NULL;
		compress_statement(result, &binary_table_defn,
				   &binary_table_defn_length); /* sets "binary_table_defn" to "malloc"ed storage */
		assert(NULL != binary_table_defn);
		status = store_table_definition(&table_subs[0], binary_table_defn, binary_table_defn_length, FALSE);
		free(binary_table_defn); /* free buffer that was "malloc"ed in "compress_statement" */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, TRUE, &cursor_ydb_buff);
		/* Cleanup any memory allocations in "parse_line()" of this iteration before moving on to next iteration
		 * to avoid memory buildup that can happen if we have to process thousands of binary definitions.
		 */
		OCTO_CFREE(memory_chunks);
		DELETE_QUERY_PARAMETER_CURSOR_LVN(&cursor_ydb_buff);
	}
	ERASE_INPUT_BUFFER; /* Clear history of all "parse_line()" processing related to binary function upgrade to avoid
			     * confusion when we next proceed to run real queries.
			     */
	CLEANUP_TABLE_BUFF(table_buff);
	return YDB_OK;
}
