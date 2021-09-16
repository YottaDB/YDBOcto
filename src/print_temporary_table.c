/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#include "template_helpers.h"

/**
 * Iterates over the last output of the plan and prints it to the screen
 */
int print_temporary_table(SqlStatement *stmt, ydb_long_t cursorId, void *parms, char *plan_name, PSQL_MessageTypeT msg_type) {
	int32_t	     status;
	ydb_buffer_t cursor_buffers[7];
	char	     cursorId_str[INT64_TO_STRING_MAX];
	ydb_buffer_t plan_meta_buffers[6];
	char	     col_num_str[INT16_TO_STRING_MAX];

	UNUSED(parms);
	UNUSED(msg_type);
	INFO(INFO_ENTERING_FUNCTION, "print_temporary_table");

	if (set_STATEMENT == stmt->type) {
		SqlSetStatement *set_stmt;
		SqlValue *	 runtime_variable_stmt, *runtime_value_stmt;

		UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
		UNPACK_SQL_STATEMENT(runtime_value_stmt, set_stmt->value, value);
		UNPACK_SQL_STATEMENT(runtime_variable_stmt, set_stmt->variable, value);

		// SET a runtime variable to a specified value
		status
		    = set_parameter_in_pg_settings(runtime_variable_stmt->v.string_literal, runtime_value_stmt->v.string_literal);
		return status;
	}
	if (show_STATEMENT == stmt->type) {
		SqlShowStatement *show_stmt;
		SqlValue *	  runtime_variable;
		ydb_buffer_t	  value_buffer;
		char *		  parameter_value;

		// SHOW a runtime variable value
		UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
		UNPACK_SQL_STATEMENT(runtime_variable, show_stmt->variable, value);
		parameter_value = get_parameter_from_pg_settings(&runtime_variable->v.string_literal, &value_buffer);
		if (NULL != parameter_value) {
			fprintf(stdout, "%.*s\n", value_buffer.len_used, value_buffer.buf_addr);
			free(parameter_value);
			return YDB_OK;
		} else {
			return 1;
		}
	}
	YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_buffers[0]);
	snprintf(cursorId_str, INT64_TO_STRING_MAX, "%ld", cursorId);
	YDB_STRING_TO_BUFFER(cursorId_str, &cursor_buffers[1]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_KEYS, &cursor_buffers[2]);

	YDB_STRING_TO_BUFFER(config->global_names.octo, &plan_meta_buffers[0]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &plan_meta_buffers[1]);
	YDB_STRING_TO_BUFFER(plan_name, &plan_meta_buffers[2]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_KEY, &plan_meta_buffers[3]);
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&cursor_buffers[3], INT64_TO_STRING_MAX);
	status = ydb_get_s(&plan_meta_buffers[0], 3, &plan_meta_buffers[1], &cursor_buffers[3]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&cursor_buffers[3]);
		if (NULL != memory_chunks) {
			OCTO_CFREE(memory_chunks);
		}
		ERROR(ERR_DATABASE_FILES_OOS, "");
		return 1;
	}

	if (select_STATEMENT == stmt->type) {
		ydb_buffer_t value_buffer;

		YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_COLUMNS, &plan_meta_buffers[3]);
		plan_meta_buffers[4].buf_addr = col_num_str;
		plan_meta_buffers[4].len_alloc = sizeof(col_num_str);
		plan_meta_buffers[4].len_used = 0;
		YDB_LITERAL_TO_BUFFER(OCTOLIT_NAME, &plan_meta_buffers[5]);

		OCTO_MALLOC_NULL_TERMINATED_BUFFER(&value_buffer, OCTO_INIT_BUFFER_LEN);
		// Retrieve the row ID for the given output key
		for (;;) { /* Note: This is a dummy for loop to be able to use "break" for various error codepaths */
			int16_t num_columns;
			int64_t tmp_long, num_rows;
			int	colnum;

			/* Print row header */
			status = ydb_get_s(plan_meta_buffers, 3, &plan_meta_buffers[1], &plan_meta_buffers[4]);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				assert(FALSE);
				break;
			}
			assert(plan_meta_buffers[4].len_alloc > plan_meta_buffers[4].len_used);
			plan_meta_buffers[4].buf_addr[plan_meta_buffers[4].len_used] = '\0';
			tmp_long = strtol(plan_meta_buffers[4].buf_addr, NULL, 10);
			if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
				num_columns = (int16_t)tmp_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol");
				status = 1; /* some non-zero value chosen here so status reflects an error */
				break;
			}
			for (colnum = 1; colnum <= num_columns; colnum++) {
				plan_meta_buffers[4].len_used
				    = snprintf(plan_meta_buffers[4].buf_addr, plan_meta_buffers[4].len_alloc, "%d", colnum);
				status = ydb_get_s(&plan_meta_buffers[0], 5, &plan_meta_buffers[1], &value_buffer);
				// Expand value_buffer allocation until it's large enough to store the retrieved row value
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
					status = ydb_get_s(&plan_meta_buffers[0], 5, &plan_meta_buffers[1], &value_buffer);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					break;
				}
				if (1 < colnum) {
					fprintf(stdout, "|");
				}
				fprintf(stdout, "%.*s", value_buffer.len_used, value_buffer.buf_addr);
			}
			fprintf(stdout, "\n");
			YDB_LITERAL_TO_BUFFER("", &cursor_buffers[4]);
			YDB_LITERAL_TO_BUFFER("", &cursor_buffers[5]);
			OCTO_MALLOC_NULL_TERMINATED_BUFFER(&cursor_buffers[6], INT64_TO_STRING_MAX);
			/* Print row values if any */
			num_rows = 0;
			for (;;) {
				status = ydb_subscript_next_s(&cursor_buffers[0], 6, &cursor_buffers[1], &cursor_buffers[6]);
				if (YDB_ERR_NODEEND == status) {
					status = YDB_OK;
					break;
				}
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					break;
				}
				status = ydb_get_s(&cursor_buffers[0], 6, &cursor_buffers[1], &value_buffer);
				// Expand value_buffer allocation until it's large enough to store the retrieved row value
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
					status = ydb_get_s(&cursor_buffers[0], 6, &cursor_buffers[1], &value_buffer);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					break;
				}
				value_buffer.buf_addr[value_buffer.len_used] = '\0';
				print_result_row(&value_buffer);
				num_rows++;
			}
			YDB_FREE_BUFFER(&cursor_buffers[6]);
			/* Print number of rows */
			fprintf(stdout, "(%lld %s)\n", (long long int)num_rows, (1 == num_rows) ? "row" : "rows");
			fflush(stdout);
			break;
		}
		YDB_FREE_BUFFER(&value_buffer);
	} else {
		/* This is not a SELECT statement type (e.g. INSERT INTO, DELETE FROM statement etc.).
		 * In that case, there are no result rows to send.
		 * Just print the number of rows inserted/deleted etc.
		 * Note: Below code is somewhat similar to that in "make_command_complete.c".
		 */
		char command_tag[MAX_TAG_LEN];
		int  row_count;

		row_count = get_row_count_from_cursorId(cursorId);
		snprintf(command_tag, MAX_TAG_LEN, "%s %d",
			 (insert_STATEMENT == stmt->type) ? INSERT_COMMAND_TAG : DELETE_COMMAND_TAG, row_count);
		/* Print number of rows */
		PRINT_COMMAND_TAG(command_tag);
		status = YDB_OK;
	}
	YDB_FREE_BUFFER(&cursor_buffers[3]);
	if (NULL != memory_chunks) {
		// Memory chunks are no longer needed after the query has been processed, so free them here.
		OCTO_CFREE(memory_chunks);
	}
	return (YDB_OK != status);
}
