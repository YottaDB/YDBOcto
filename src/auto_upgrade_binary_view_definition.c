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

#define CLEANUP_TABLE_BUFF(TABLE_BUFF)           \
	{                                        \
		YDB_FREE_BUFFER(&TABLE_BUFF[0]); \
		YDB_FREE_BUFFER(&TABLE_BUFF[1]); \
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

int auto_upgrade_binary_view_definition(void) {
	// Call M routine to form a no-dependency view list
	int status;
	status = ydb_ci("_ydboctoCreateViewsUpgradeNamesList");
	if (YDB_OK != status) {
		YDB_ERROR_CHECK(status);
		return status;
	}
	/* The above call would have created the following lvn
	 * 	%viewssortednames=no_of_nodes
	 * 	%viewssortednames(1)="view_name"
	 */
	// Upgrade the views in the sorted dependency view list
	ydb_buffer_t view_list_name_buffer, table_subs[2], *table_buff;

	// Setup variable name
	YDB_STRING_TO_BUFFER(OCTOLIT_YDBOCTOVIEWSORTEDNAMES, &view_list_name_buffer);
	// Setup first subscript
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&table_subs[0], MAX_DEFINITION_FRAGMENT_SIZE); /* to store row index */
	table_subs[0].len_used = 0;
	// Setup return value
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&table_subs[1], YDB_MAX_KEY_SZ); /* to store the view name */
	table_buff = &table_subs[0]; /* Note down that this buffer needs to be freed in case of error code path */
	while (TRUE) {
		// Get view name to upgrade
		// 1. Get first subscript the index
		//    $ORDER(view_list_name(subs1))
		status = ydb_subscript_next_s(&view_list_name_buffer, 1, &table_subs[0], &table_subs[0]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		// 2. Get second subscript the view name
		//    view_list_name(subs1)=value
		//    $GET(view_list_name(subs1))
		table_subs[1].len_used = 0; // Reset subscript for ydb_subscript_next_s
		status = ydb_get_s(&view_list_name_buffer, 1, &table_subs[0], &table_subs[1]);
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);

		// Upgrade the view
		status = auto_upgrade_binary_table_or_view_definition_helper(&table_subs[1]);
		if (YDB_OK != status) {
			CLEANUP_AND_RETURN(status, table_buff, NULL, FALSE, NULL);
		}
		/* Set %ydboctoViewCreated(viewname) to "" so that find_view_or_table() can know that `viewname` has
		 * been upgraded and its binary definition can be used.
		 */
		// Setup LVN buffers
		ydb_buffer_t view_created_list_name_buffer;
		YDB_STRING_TO_BUFFER(OCTOLIT_YDBOCTOVIEWCREATED, &view_created_list_name_buffer);

		// Set LVN
		status = ydb_set_s(&view_created_list_name_buffer, 1, &table_subs[1], NULL);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);
	}
	ERASE_INPUT_BUFFER; /* Clear history of all "parse_line()" processing related to binary function upgrade to avoid
			     * confusion when we next proceed to run real queries.
			     */
	CLEANUP_TABLE_BUFF(table_buff);
	return YDB_OK;
}
