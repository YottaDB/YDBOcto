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

#define CLEANUP_TABLE_BUFF(TABLE_BUFF)           \
	{                                        \
		YDB_FREE_BUFFER(&TABLE_BUFF[0]); \
		YDB_FREE_BUFFER(&TABLE_BUFF[2]); \
	}

#define CLEANUP_AND_RETURN(STATUS, TABLE_BUFF, FREE_MEMORY_CHUNK) \
	{                                                         \
		if (NULL != TABLE_BUFF) {                         \
			CLEANUP_TABLE_BUFF(TABLE_BUFF);           \
		}                                                 \
		if (FREE_MEMORY_CHUNK) {                          \
			OCTO_CFREE(memory_chunks);                \
		}                                                 \
		return STATUS;                                    \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, TABLE_BUFF, FREE_MEMORY_CHUNK)    \
	{                                                                          \
		if (YDB_OK != STATUS) {                                            \
			YDB_ERROR_CHECK(STATUS);                                   \
			CLEANUP_AND_RETURN(STATUS, TABLE_BUFF, FREE_MEMORY_CHUNK); \
		}                                                                  \
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
	 * and set the following nodes
	 *	^%ydboctoschema(table_name,OCTOLIT_BINARY,...)
	 *	^%ydboctoschema(table_name,OCTOLIT_LENGTH).
	 */
	YDB_MALLOC_BUFFER(&table_subs[0], YDB_MAX_KEY_SZ); /* to store the table name */
	table_subs[0].len_used = 0;
	YDB_MALLOC_BUFFER(&table_subs[2], YDB_MAX_STR); /* to store the return */
	table_buff = &table_subs[0]; /* Note down that this buffer needs to be freed in case of error code path */
	while (TRUE) {
		char *	     binary_table_defn; /* pointer to the binary table definition */
		int	     binary_table_defn_length;
		ydb_buffer_t cursor_ydb_buff;
		ydb_long_t   cursorId;
		char	     cursor_buffer[INT64_TO_STRING_MAX];
		long long    table_oid;
		SqlTable *   table;

		status = ydb_subscript_next_s(&schema_global, 1, &table_subs[0], &table_subs[0]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, FALSE);
		/* Get the "CREATE TABLE" query corresponding to "table_buff" and recompute binary definition.
		 *	^%ydboctoschema(table_name,OCTOLIT_TEXT)
		 */
		YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &table_subs[1]);
		status = ydb_get_s(&schema_global, 2, &table_subs[0], &table_subs[2]);
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_STR above */
		if (YDB_ERR_GVUNDEF == status) {
			/* For some prior Octo commits, the text definition was stored in a subscript "t" (instead of "text").
			 * So check that too.
			 */
			YDB_STRING_TO_BUFFER(OCTOLIT_T, &table_subs[1]);
			status = ydb_get_s(&schema_global, 2, &table_subs[0], &table_subs[2]);
			assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_STR above */
			if (YDB_OK == status) {
				/* There are other issues because of which auto upgrade is not possible.
				 * For example, the DDL had "$C(1)" in the DELIM (table level) for the CREATE TABLE.
				 * It is not considered necessary to try and auto upgrade such text definitions for now.
				 * So issue an error that asks the user to run a manual upgrade.
				 */
				ERROR(ERR_AUTO_UPGRADE, NULL);
				CLEANUP_AND_RETURN(1, table_buff, FALSE);
			}
		}
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, FALSE);
		COPY_QUERY_TO_INPUT_BUFFER(table_subs[2].buf_addr, (int)table_subs[2].len_used, NEWLINE_NEEDED_FALSE);

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
			CLEANUP_AND_RETURN(1, table_buff, TRUE);
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
			CLEANUP_AND_RETURN(1, table_buff, TRUE);
		}
		/* Get OID of the table name (from below gvn) as we need that OID to store in the binary table definition.
		 *	^%ydboctoschema(table_name,OCTOLIT_PG_CLASS)=TABLEOID
		 */
		YDB_STRING_TO_BUFFER(OCTOLIT_PG_CLASS, &table_subs[1]);
		status = ydb_get_s(&schema_global, 2, &table_subs[0], &table_subs[2]);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, TRUE);
		assert(table_subs[2].len_used < table_subs[2].len_alloc);
		table_subs[2].buf_addr[table_subs[2].len_used] = '\0'; /* null terminate for "strtoll" */
		table_oid = strtoll(table_subs[2].buf_addr, NULL, 10);
		if ((LLONG_MIN == table_oid) || (LLONG_MAX == table_oid)) {
			ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), table_subs[2].buf_addr);
			CLEANUP_AND_RETURN(1, table_buff, TRUE);
		}
		assert(create_table_STATEMENT == result->type);
		UNPACK_SQL_STATEMENT(table, result, create_table);
		/* Store OID in the SqlTable structure so it goes in the binary table definition as part of "compress_statement" */
		table->oid = table_oid;

		binary_table_defn = NULL;
		compress_statement(result, &binary_table_defn,
				   &binary_table_defn_length); /* sets "binary_table_defn" to "malloc"ed storage */
		assert(NULL != binary_table_defn);
		status = store_binary_table_definition(&table_subs[0], binary_table_defn, binary_table_defn_length);
		free(binary_table_defn); /* free buffer that was "malloc"ed in "compress_statement" */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, TRUE);
	}
	CLEANUP_TABLE_BUFF(table_buff);
	return YDB_OK;
}
