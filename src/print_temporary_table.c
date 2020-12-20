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
int print_temporary_table(SqlStatement *stmt, int cursor_id, void *parms, char *plan_name, boolean_t send_row_description) {
	ydb_buffer_t *session_buffers;
	ydb_buffer_t  value_buffer;
	SqlValue *    runtime_variable;
	int32_t	      status;
	ydb_buffer_t  cursor_buffers[7];
	char	      cursor_id_str[INT64_TO_STRING_MAX];

	UNUSED(parms);
	UNUSED(send_row_description);
	INFO(INFO_ENTERING_FUNCTION, "print_temporary_table");

	YDB_MALLOC_BUFFER(&value_buffer, OCTO_INIT_BUFFER_LEN);
	value_buffer.len_alloc--; // Leave room for null terminator
	if (set_STATEMENT == stmt->type) {
		SqlSetStatement *set_stmt;
		SqlValue *	 runtime_value;
		int32_t		 done;

		// SET a runtime variable to a specified value by updating the appropriate session LVN
		UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
		UNPACK_SQL_STATEMENT(runtime_value, set_stmt->value, value);
		UNPACK_SQL_STATEMENT(runtime_variable, set_stmt->variable, value);
		YDB_COPY_STRING_TO_BUFFER(runtime_value->v.string_literal, &value_buffer, done);
		if (!done) {
			YDB_FREE_BUFFER(&value_buffer);
			YDB_MALLOC_BUFFER(&value_buffer, strlen(runtime_value->v.string_literal) + 1); // Null terminator
			YDB_COPY_STRING_TO_BUFFER(runtime_value->v.string_literal, &value_buffer, done);
			assert(done);
		}
		session_buffers = make_buffers(config->global_names.session, 3, OCTOLIT_0, OCTOLIT_VARIABLES,
					       runtime_variable->v.string_literal);
		status = ydb_set_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
		free(session_buffers);
		YDB_FREE_BUFFER(&value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			return 1;
		return 0;
	}
	if (show_STATEMENT == stmt->type) {
		SqlShowStatement *show_stmt;

		// Attempt to GET the value of the specified runtime variable from the appropriate session LVN
		UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
		UNPACK_SQL_STATEMENT(runtime_variable, show_stmt->variable, value);
		session_buffers = make_buffers(config->global_names.session, 3, OCTOLIT_0, OCTOLIT_VARIABLES,
					       runtime_variable->v.string_literal);
		status = ydb_get_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		// If the variable isn't defined for the session, attempt to pull the value from the Octo GVN
		if (YDB_OK != status) {
			ydb_buffer_t *octo_buffers;

			octo_buffers
			    = make_buffers(config->global_names.octo, 2, OCTOLIT_VARIABLES, runtime_variable->v.string_literal);
			status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
				status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
				assert(YDB_ERR_INVSTRLEN != status);
			}
			/* If the variable isn't defined on the Octo GVN, the variable isn't defined at all.
			 * In this case we will return an empty string.
			 */
			if (YDB_OK != status) {
				value_buffer.buf_addr[0] = '\0';
				value_buffer.len_used = 0;
			}
			free(octo_buffers);
		}
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		fprintf(stdout, "%s\n", value_buffer.buf_addr);
		YDB_FREE_BUFFER(&value_buffer);
		free(session_buffers);
		return 0;
	}
	YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_buffers[0]);
	snprintf(cursor_id_str, INT64_TO_STRING_MAX, "%d", cursor_id);
	YDB_STRING_TO_BUFFER(cursor_id_str, &cursor_buffers[1]);
	if (select_STATEMENT == stmt->type) {
		ydb_buffer_t plan_meta_buffers[6];
		char	     col_num_str[INT16_TO_STRING_MAX];

		YDB_LITERAL_TO_BUFFER(OCTOLIT_KEYS, &cursor_buffers[2]);

		YDB_STRING_TO_BUFFER(config->global_names.octo, &plan_meta_buffers[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &plan_meta_buffers[1]);
		YDB_STRING_TO_BUFFER(plan_name, &plan_meta_buffers[2]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_KEY, &plan_meta_buffers[3]);
		YDB_MALLOC_BUFFER(&cursor_buffers[3], INT64_TO_STRING_MAX);
		status = ydb_get_s(&plan_meta_buffers[0], 3, &plan_meta_buffers[1], &cursor_buffers[3]);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&cursor_buffers[3]);
			YDB_FREE_BUFFER(&value_buffer);
			if (NULL != memory_chunks) {
				OCTO_CFREE(memory_chunks);
			}
			ERROR(ERR_DATABASE_FILES_OOS, "");
			return 1;
		}
		YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_COLUMNS, &plan_meta_buffers[3]);
		plan_meta_buffers[4].buf_addr = col_num_str;
		plan_meta_buffers[4].len_alloc = sizeof(col_num_str);
		plan_meta_buffers[4].len_used = 0;
		YDB_LITERAL_TO_BUFFER(OCTOLIT_NAME, &plan_meta_buffers[5]);

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
			YDB_MALLOC_BUFFER(&cursor_buffers[6], INT64_TO_STRING_MAX);
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
		YDB_FREE_BUFFER(&cursor_buffers[3]);
	} else {
		/* This is not a SELECT statement type (e.g. INSERT INTO, DELETE FROM statement etc.).
		 * In that case, there are no result rows to send.
		 */
		status = YDB_OK;
	}
	// Cleanup tables
	ydb_delete_s(&cursor_buffers[0], 1, &cursor_buffers[1], YDB_DEL_TREE);
	YDB_FREE_BUFFER(&value_buffer);
	if (NULL != memory_chunks) {
		// Memory chunks are no longer needed after the query has been processed, so free them here.
		OCTO_CFREE(memory_chunks);
	}
	return (YDB_OK != status);
}
