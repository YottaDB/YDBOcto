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

/* Automatically upgrade all binary table definitions.
 * Returns YDB_OK on success and a non-zero (YDB_ERR_* status code or 1) on errors.
 * Note: The below logic is similar to that in "src/auto_upgrade_binary_function_definition.c".
 */
int auto_upgrade_binary_table_definition(void) {
	ydb_buffer_t schema_global, table_subs[1], *table_buff;
	int	     status;

	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	/* $order through ^%ydboctoschema(table_name) and for each table_name upgrade its binary definition */
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&table_subs[0], YDB_MAX_KEY_SZ); /* to store the table name */
	table_subs[0].len_used = 0;
	table_buff = &table_subs[0]; /* Note down that this buffer needs to be freed in case of error code path */
	while (TRUE) {
		// Get relation name
		status = ydb_subscript_next_s(&schema_global, 1, &table_subs[0], &table_subs[0]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		assert(YDB_ERR_INVSTRLEN != status); /* because we allocated YDB_MAX_KEY_SZ above */
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);
		// Check if this is a view
		boolean_t    is_view = FALSE;
		ydb_buffer_t node_value;
		char	     node_value_buff[OCTO_MAX_IDENT + 1];
		node_value.buf_addr = node_value_buff;
		node_value.len_alloc = sizeof(node_value_buff) - 1; /* reserve 1 byte for null terminator */
		status = ydb_get_s(&schema_global, 1, &table_subs[0], &node_value);
		switch (status) {
		case YDB_OK:
			/* A node of the given name exists in ^%ydboctoschema
			 * Check if the value stored is a view by looking at its value.
			 */
			assert(node_value.len_alloc > node_value.len_used); /* Ensure space for null terminator */
			node_value.buf_addr[node_value.len_used] = '\0';    /* Null terminate string */
			is_view = (0 == strcmp(node_value.buf_addr, OCTOLIT_VIEW)) ? TRUE : FALSE;
			break;
		case YDB_ERR_GVUNDEF:
			/* This code block is retained to facilitate older implementation of auto-upgrade logic
			 * where ^%ydboctoschema(name)="table" or ^%ydboctoschema(name)="view" didn't exist.
			 * In this case nodes of the following form will exist ^%ydboctoschema(table_name,OCTOLIT_TEXT),
			 * invoke the helper with the `table_name` and it will take care of the upgrade.
			 */
			is_view = FALSE;
			break;
		default:
			YDB_ERROR_CHECK(status);
			return 1;
			break;
		}
		if (is_view) {
			// Check if a `viewdependency` node exist for this view
			ydb_buffer_t gvn_subs[4];
			YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
			YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWDEPENDENCY, &gvn_subs[1]);
			gvn_subs[2] = table_subs[0];

			unsigned int ret_value;
			status = ydb_data_s(&gvn_subs[0], 2, &gvn_subs[1], &ret_value);
			assert(YDB_OK == status);
			CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);
			/* It is possible for views to exist with `ret_value` being `0` in case VALUES clause alone exists in
			 * the view definition or its join. If the view definition made use of table or a function (even
			 * `select 1` makes use of `octoOneRowTable`) `ret_value` would be non-zero.
			 */
			if (!ret_value) {
				// VALUES dependency exists
				/* Store the following gvn node now, this ensures auto upgrade will include this view also in the
				 * list of views it upgrades. Refer to the logic in src/aux/_ydboctoViewsUpgrade.m to know how
				 * this happens.
				 * Gvn - ^%ydboctoocto("viewdependency","V2","values");
				 */
				YDB_LITERAL_TO_BUFFER(OCTOLIT_VALUES, &gvn_subs[3]);
				status = ydb_set_s(&gvn_subs[0], 3, &gvn_subs[1], NULL);
				assert(YDB_OK == status);
				CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, table_buff, NULL, FALSE, NULL);
			}
			// Skip views upgrade it will be done later
			continue;
		}
		status = auto_upgrade_binary_table_or_view_definition_helper(&table_subs[0], FALSE);
		if (YDB_OK != status) {
			CLEANUP_AND_RETURN(status, table_buff, NULL, FALSE, NULL);
		}
	}
	ERASE_INPUT_BUFFER; /* Clear history of all "parse_line()" processing related to binary function upgrade to avoid
			     * confusion when we next proceed to run real queries.
			     */
	CLEANUP_TABLE_BUFF(table_buff);
	return YDB_OK;
}
