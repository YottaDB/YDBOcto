/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
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
#define CLEANUP_TABLE_BUFF(TABLE_BUFF) \
	{ YDB_FREE_BUFFER(&TABLE_BUFF[0]); }
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
/* Given a table or view name, text defintion is fetched from the schema global.
 * Input:
 *	ydb_buffer_t *view_or_table_name: table/view name buffer used to fetch the text definition
 *
 *	char **text_definition:
 *		NULL to copy text definition to query input buffer ("input_buffer_combined")
 *		non-NULL to store text definition to the location pointed by it
 * Output:
 *	0 for success, -1 for error
 */
int get_table_or_view_text_definition(ydb_buffer_t *view_or_table_name, char **text_definition) {
	ydb_buffer_t table_or_view_subs[3], *table_buff;
	// First subscript
	table_or_view_subs[0].buf_addr = view_or_table_name->buf_addr;
	table_or_view_subs[0].len_alloc = view_or_table_name->len_alloc;
	table_or_view_subs[0].len_used = view_or_table_name->len_used;
	// Second subscript
	YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &table_or_view_subs[1]);
	// Return variable
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&table_or_view_subs[2], MAX_DEFINITION_FRAGMENT_SIZE); /* to store the return */
	table_buff = &table_or_view_subs[2]; /* Note down that this buffer needs to be freed in case of error code path */
	// schema_global
	ydb_buffer_t schema_global;
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	ydb_buffer_t text_defn_buff;
	char	     text_defn_str[MAX_DEFINITION_FRAGMENT_SIZE];
	char *	     text_defn = NULL;
	long	     text_defn_len, cur_len;

	text_defn_buff.buf_addr = text_defn_str;
	text_defn_buff.len_alloc = sizeof(text_defn_str);
	text_defn_buff.len_used = 0;
	// Get the length of the full text table definition
	YDB_STRING_TO_BUFFER(OCTOLIT_TEXT_LENGTH, &table_or_view_subs[1]);
	int status;
	status = ydb_get_s(&schema_global, 2, &table_or_view_subs[0], &text_defn_buff);
	assert(YDB_ERR_INVSTRLEN != status); /* because we allocated MAX_DEFINITION_FRAGMENT_SIZE above */
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);

	// Null terminate for strtoll
	text_defn_buff.buf_addr[text_defn_buff.len_used] = '\0';
	text_defn_len = strtoll(text_defn_buff.buf_addr, NULL, 10);

	if ((LLONG_MIN == text_defn_len) || (LLONG_MAX == text_defn_len)) {
		ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), text_defn_buff.buf_addr);
		CLEANUP_AND_RETURN(1, table_buff, NULL, FALSE, NULL);
	}
	YDB_STRING_TO_BUFFER(OCTOLIT_TEXT, &table_or_view_subs[1]); // Reset subscript
	cur_len = 0;
	text_defn = (char *)malloc(sizeof(char) * text_defn_len + 1);
	table_or_view_subs[2].len_used = 0; // Reset subscript for ydb_subscript_next_s
	do {
		status = ydb_subscript_next_s(&schema_global, 3, &table_or_view_subs[0], &table_or_view_subs[2]);
		table_or_view_subs[2].buf_addr[table_or_view_subs[2].len_used] = '\0';
		if (YDB_ERR_NODEEND == status) {
			status = YDB_OK;
			break;
		}
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, text_defn, FALSE, NULL);
		status = ydb_get_s(&schema_global, 3, &table_or_view_subs[0], &text_defn_buff);
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated MAX_DEFINITION_FRAGMENT_SIZE above */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, text_defn, FALSE, NULL);
		memcpy(&text_defn[cur_len], text_defn_buff.buf_addr, text_defn_buff.len_used);
		cur_len += text_defn_buff.len_used;
	} while (cur_len <= text_defn_len);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, text_defn, FALSE, NULL);
	if (NULL == text_definition) {
		COPY_QUERY_TO_INPUT_BUFFER(text_defn, text_defn_len, NEWLINE_NEEDED_FALSE);
		free(text_defn);
	} else {
		text_defn[text_defn_len] = '\0';
		*text_definition = text_defn;
	}
	CLEANUP_TABLE_BUFF(table_buff);
	return status;
}
