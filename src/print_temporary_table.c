/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "helpers.h"

#include "physical_plan.h"

/**
 * Iterates over the last output of the plan and prints it to the screen
 */
int print_temporary_table(SqlStatement *stmt, int cursor_id, void *parms, char *plan_name, boolean_t send_row_description) {
	SqlShowStatement	*show_stmt;
	SqlSetStatement		*set_stmt;
	ydb_buffer_t		*session_buffers, *outkey_buffers, *cursor_buffers, *octo_buffers;
	ydb_buffer_t		value_buffer;
	SqlValue		*val1, *val2;
	int32_t			status, done;
	char			cursor_id_str[INT64_TO_STRING_MAX];

	UNUSED(parms);
	UNUSED(send_row_description);
	INFO(CUSTOM_ERROR, "%s", "print_temporary_table()");

	YDB_MALLOC_BUFFER(&value_buffer, MAX_STR_CONST);
	if (stmt->type == set_STATEMENT) {
		session_buffers = make_buffers(config->global_names.session, 3, "0", "variables", "");
		UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
		UNPACK_SQL_STATEMENT(val1, set_stmt->value, value);
		UNPACK_SQL_STATEMENT(val2, set_stmt->variable, value);
		YDB_COPY_STRING_TO_BUFFER(val2->v.string_literal, &value_buffer, done);
		if (!done) {
			ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
			free(session_buffers);
			YDB_FREE_BUFFER(&value_buffer);
			return 1;
		}
		status = ydb_set_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
		free(session_buffers);
		YDB_FREE_BUFFER(&value_buffer);
		if (YDB_OK != status)
			return 1;
		return 0;
	}
	if (stmt->type == show_STATEMENT) {
		session_buffers = make_buffers(config->global_names.session, 3, "0", "variables", "");
		UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
		UNPACK_SQL_STATEMENT(val1, show_stmt->variable, value);
		YDB_COPY_STRING_TO_BUFFER(val1->v.string_literal, &value_buffer, done);
		if (!done) {
			ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
			YDB_FREE_BUFFER(&value_buffer);
			free(session_buffers);
			return 1;
		}
		status = ydb_get_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
		if (YDB_OK != status) {
			octo_buffers = make_buffers(config->global_names.octo, 3, "variables", "", "");
			YDB_MALLOC_BUFFER(&octo_buffers[2], MAX_STR_CONST);
			YDB_COPY_STRING_TO_BUFFER(val1->v.string_literal, &octo_buffers[2], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
				YDB_FREE_BUFFER(&octo_buffers[2]);
				YDB_FREE_BUFFER(&value_buffer);
				free(session_buffers);
				free(octo_buffers);
				return 1;
			}
			status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
			if (YDB_OK != status) {
				value_buffer.buf_addr[0] = '\0';
				value_buffer.len_used = 0;
			}
			free(octo_buffers);
			YDB_FREE_BUFFER(&octo_buffers[2]);
		}
		fprintf(stdout, "%s\n", value_buffer.buf_addr);
		YDB_FREE_BUFFER(&value_buffer);
		free(session_buffers);
		return 0;
	}

	snprintf(cursor_id_str, INT64_TO_STRING_MAX, "%d", cursor_id);
	cursor_buffers = make_buffers(config->global_names.cursor, 6, cursor_id_str, "keys", "", "", "", "");
	outkey_buffers = make_buffers("^%ydboctoocto", 3, "plan_metadata", plan_name, "output_key");

	YDB_MALLOC_BUFFER(&cursor_buffers[3], INT64_TO_STRING_MAX);
	status = ydb_get_s(&outkey_buffers[0], 3, &outkey_buffers[1], &cursor_buffers[3]);
	free(outkey_buffers);
	YDB_ERROR_CHECK(status);
	if(YDB_OK != status) {
		YDB_FREE_BUFFER(&cursor_buffers[3]);
		free(cursor_buffers);
		if(memory_chunks != NULL) {
			OCTO_CFREE(memory_chunks);
		}
		ERROR(ERR_DATABASE_FILES_OOS, "");
		return 1;
	}

	// Retrieve the row ID for the given output key
	YDB_MALLOC_BUFFER(&cursor_buffers[6], INT64_TO_STRING_MAX);
	status = ydb_subscript_next_s(&cursor_buffers[0], 6, &cursor_buffers[1], &cursor_buffers[6]);
	if (YDB_ERR_NODEEND == status)
		return 0;
	YDB_ERROR_CHECK(status);
	if (YDB_OK == status) {
		while (0 != strncmp("", cursor_buffers[6].buf_addr, MAX_STR_CONST)) {
			status = ydb_get_s(&cursor_buffers[0], 6, &cursor_buffers[1], &value_buffer);
			// Expand value_buffer allocation until it's large enough to store the row value retrieved from the database
			while (YDB_ERR_INVSTRLEN == status) {
				int newsize = value_buffer.len_used;
				YDB_FREE_BUFFER(&value_buffer);
				YDB_MALLOC_BUFFER(&value_buffer, newsize + 1);
				value_buffer.len_alloc--;
				status = ydb_get_s(&cursor_buffers[0], 6, &cursor_buffers[1], &value_buffer);
			}
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
			value_buffer.buf_addr[value_buffer.len_used] = '\0';
			fprintf(stdout, "%s\n", value_buffer.buf_addr);
			status = ydb_subscript_next_s(&cursor_buffers[0], 6, &cursor_buffers[1], &cursor_buffers[6]);
			if(YDB_ERR_NODEEND == status) {
				status = YDB_OK;
				break;
			}
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
		}
		fflush(stdout);
	}
	// Cleanup tables
	ydb_delete_s(&cursor_buffers[0], 1, &cursor_buffers[1], YDB_DEL_TREE);

	YDB_FREE_BUFFER(&cursor_buffers[3]);
	YDB_FREE_BUFFER(&cursor_buffers[6]);
	YDB_FREE_BUFFER(&value_buffer);
	free(cursor_buffers);
	if(memory_chunks != NULL) {
		OCTO_CFREE(memory_chunks);
	}
	return (YDB_OK != status);
}
