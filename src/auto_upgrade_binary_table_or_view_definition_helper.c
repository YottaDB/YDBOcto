/****************************************************************
 *								*
 * Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	*
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

#define CLEANUP_VIEW_OR_TABLE_BUFF(VIEW_OR_TABLE_BUFF) \
	{ YDB_FREE_BUFFER(&VIEW_OR_TABLE_BUFF[0]); }

#define CLEANUP_AND_RETURN(STATUS, VIEW_OR_TABLE_BUFF, TEXT_DEFN, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF) \
	{                                                                                             \
		if (NULL != VIEW_OR_TABLE_BUFF) {                                                     \
			CLEANUP_VIEW_OR_TABLE_BUFF(VIEW_OR_TABLE_BUFF);                               \
		}                                                                                     \
		if (NULL != TEXT_DEFN) {                                                              \
			free(TEXT_DEFN);                                                              \
		}                                                                                     \
		if (FREE_MEMORY_CHUNK) {                                                              \
			OCTO_CFREE(memory_chunks);                                                    \
		}                                                                                     \
		if (NULL != CURSOR_YDB_BUFF) {                                                        \
			DELETE_QUERY_PARAMETER_CURSOR_LVN(CURSOR_YDB_BUFF);                           \
		}                                                                                     \
		return STATUS;                                                                        \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, VIEW_OR_TABLE_BUFF, TEXT_DEFN, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF)    \
	{                                                                                                              \
		if (YDB_OK != STATUS) {                                                                                \
			YDB_ERROR_CHECK(STATUS);                                                                       \
			CLEANUP_AND_RETURN(STATUS, VIEW_OR_TABLE_BUFF, TEXT_DEFN, FREE_MEMORY_CHUNK, CURSOR_YDB_BUFF); \
		}                                                                                                      \
	}

int auto_upgrade_binary_table_or_view_definition_helper(ydb_buffer_t *view_or_table_name) {
	/* $order through ^%ydboctoschema(view_or_table_name) and for each view_or_table_name, get CREATE TABLE/VIEW statement from
	 *	^%ydboctoschema(view_or_table_name,OCTOLIT_TEXT)
	 * OR
	 *	^%ydboctoschema(view_or_table_name,OCTOLIT_TEXT,0)
	 *	^%ydboctoschema(view_or_table_name,OCTOLIT_TEXT,1)
	 * AND set the following nodes
	 *	^%ydboctoschema(view_or_table_name,OCTOLIT_BINARY,...)
	 *	^%ydboctoschema(view_or_table_name,OCTOLIT_LENGTH).
	 * Note: We need to account for multiple node configurations for text nodes as the changes for YDBOcto#590 introduced text
	 * definition fragmentation to allow for arbitrarily long DDL definitions. However, earlier versions of Octo do not have
	 * this layout, and so we must handle both cases for backward compatibility.
	 */
	ydb_buffer_t table_or_view_subs[3], *table_or_view_buff;
	// First subscript
	table_or_view_subs[0].buf_addr = view_or_table_name->buf_addr;
	table_or_view_subs[0].len_alloc = view_or_table_name->len_alloc;
	table_or_view_subs[0].len_used = view_or_table_name->len_used;
	// Second subscript
	YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &table_or_view_subs[1]);
	// Return variable
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&table_or_view_subs[2], MAX_DEFINITION_FRAGMENT_SIZE); /* to store the return */
	table_or_view_buff = &table_or_view_subs[2]; /* Note down that this buffer needs to be freed in case of error code path */

	unsigned int data_ret;
	int	     status;
	ydb_buffer_t schema_global;
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	status = ydb_data_s(&schema_global, 2, &table_or_view_subs[0], &data_ret);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_or_view_buff, NULL, FALSE, NULL);
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
			YDB_STRING_TO_BUFFER(OCTOLIT_T, &table_or_view_subs[1]);
		}
		status = ydb_get_s(&schema_global, 2, &table_or_view_subs[0], &table_or_view_subs[2]);
		// Expand buffer if value between 32KiB (current buffer initial size) and 1MiB (buffer size in prior
		// commits)
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(table_or_view_subs[2]);
			status = ydb_get_s(&schema_global, 2, &table_or_view_subs[0], &table_or_view_subs[2]);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		if ((0 == data_ret) && (YDB_OK == status)) {
			/* There are other issues because of which auto upgrade is not possible.
			 * For example, the DDL had "$C(1)" in the DELIM (table level) for the CREATE TABLE.
			 * It is not considered necessary to try and auto upgrade such text definitions for now.
			 * So issue an error that asks the user to run a manual upgrade.
			 */
			ERROR(ERR_AUTO_UPGRADE, "");
			CLEANUP_AND_RETURN(1, table_or_view_buff, NULL, FALSE, NULL);
		}
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_or_view_buff, NULL, FALSE, NULL);
		COPY_QUERY_TO_INPUT_BUFFER(table_or_view_subs[2].buf_addr, (int)table_or_view_subs[2].len_used,
					   NEWLINE_NEEDED_FALSE);
	} else {
		/* Note: COPY_QUERY_TO_INPUT_BUFFER macro invocation is done inside "get_table_or_view_text_definition() call */
		status = get_table_or_view_text_definition(view_or_table_name, NULL);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_or_view_buff, NULL, FALSE, NULL);
	}

	/* Note: Following code is similar to that in octo.c and run_query.c */
	ParseContext parse_context;
	memset(&parse_context, 0, sizeof(parse_context));
	memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
	/* From now on, all CLEANUP_* macro calls will have TRUE as the last-but-one parameter
	 * to indicate "OCTO_CFREE(memory_chunks)" is needed for proper cleanup.
	 */
	ydb_buffer_t cursor_ydb_buff;
	char	     cursor_buffer[INT64_TO_STRING_MAX];
	cursor_ydb_buff.buf_addr = cursor_buffer;
	cursor_ydb_buff.len_alloc = sizeof(cursor_buffer);

	ydb_long_t cursorId;
	cursorId = create_cursor(&schema_global, &cursor_ydb_buff);
	if (0 > cursorId) {
		CLEANUP_AND_RETURN(1, table_or_view_buff, NULL, TRUE, NULL);
	}
	parse_context.cursorId = cursorId;
	parse_context.cursorIdString = cursor_ydb_buff.buf_addr;

	/* To print only the current query store the index for the last one
	 * then print the difference between the cur_input_index - old_input_index
	 */
	old_input_index = cur_input_index;

	// Kill View's Cache created and let `CLEANUP_AND_RETURN_IF_NOT_YDB_OK` do `YDB_ERROR_CHECK`
	INIT_VIEW_CACHE_FOR_CURRENT_QUERY(config->global_names.loadedschemas, status);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_or_view_buff, NULL, TRUE, &cursor_ydb_buff);

	SqlStatement *result;
	result = parse_line(&parse_context);
	/* From now on, all CLEANUP_* macro calls will have TRUE as the last parameter to indicate
	 * "DELETE_QUERY_PARAMETER_CURSOR_LVN " is needed to cleanup/delete any query parameter
	 * related lvn nodes and avoid lvn buildup across multiple such invocations of "parse_line()"
	 * in this "for" loop.
	 */
	if (NULL == result) {
		INFO(INFO_RETURNING_FAILURE, "parse_line");
		CLEANUP_AND_RETURN(1, table_or_view_buff, NULL, TRUE, &cursor_ydb_buff);
	}
	/* Complete view definition in case this is a create_view_STATEMENT.
	 * This helps to avoid asterisk related parsing issues. Refer to the following comment for more details
	 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1378#note_1380319421
	 * Similar code exists in run_query.c any change here needs to be done there as well
	 */
	if (create_view_STATEMENT == result->type) {
		result = view_definition(result, &parse_context);
		if (NULL == result) {
			CLEANUP_AND_RETURN(1, table_or_view_buff, NULL, TRUE, &cursor_ydb_buff);
		}
	}

#ifndef FORCE_BINARY_DEFINITION_AUTO_UPGRADE
	INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
#endif
	/* Get OID of the table name (from below gvn) as we need that OID to store in the binary table definition.
	 *	^%ydboctoschema(view_or_table_name,OCTOLIT_PG_CLASS)=TABLEOID
	 */
	YDB_STRING_TO_BUFFER(OCTOLIT_PG_CLASS, &table_or_view_subs[1]);
	status = ydb_get_s(&schema_global, 2, &table_or_view_subs[0], &table_or_view_subs[2]);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_or_view_buff, NULL, TRUE, &cursor_ydb_buff);
	assert(table_or_view_subs[2].len_used < table_or_view_subs[2].len_alloc);
	table_or_view_subs[2].buf_addr[table_or_view_subs[2].len_used] = '\0'; /* null terminate for "strtoll" */

	long long table_oid;
	table_oid = strtoll(table_or_view_subs[2].buf_addr, NULL, 10);
	if ((LLONG_MIN == table_oid) || (LLONG_MAX == table_oid)) {
		ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), table_or_view_subs[2].buf_addr);
		CLEANUP_AND_RETURN(1, table_or_view_buff, NULL, TRUE, &cursor_ydb_buff);
	}
	boolean_t is_view;
	is_view = (create_view_STATEMENT == result->type) ? TRUE : FALSE;
	if (!is_view) {
		SqlTable *table;
		UNPACK_SQL_STATEMENT(table, result, create_table);
		/* Store OID in the SqlTable structure so it goes in the binary table definition as part of
		 * "compress_statement" */
		table->oid = table_oid;
	}

	char *binary_view_or_table_defn; /* pointer to the binary table definition */
	binary_view_or_table_defn = NULL;

	int binary_view_or_table_defn_length;
	compress_statement(result, &binary_view_or_table_defn, &binary_view_or_table_defn_length,
			   is_view); /* sets "binary_view_or_table_defn" to "malloc"ed storage */
	assert(NULL != binary_view_or_table_defn);
	status = store_table_definition(&table_or_view_subs[0], binary_view_or_table_defn, binary_view_or_table_defn_length, FALSE);
	free(binary_view_or_table_defn); /* free buffer that was "malloc"ed in "compress_statement" */
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_or_view_buff, NULL, TRUE, &cursor_ydb_buff);
	/* Cleanup any memory allocations in "parse_line()" of this iteration before moving on to next iteration
	 * to avoid memory buildup that can happen if we have to process thousands of binary definitions.
	 */
	OCTO_CFREE(memory_chunks);
	DELETE_QUERY_PARAMETER_CURSOR_LVN(&cursor_ydb_buff);
	CLEANUP_VIEW_OR_TABLE_BUFF(table_or_view_buff);
	return YDB_OK;
}
