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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"
#include "physical_plan.h"
#include "helpers.h"

#define DATA_ROW_PARMS_ARRAY_INIT_ALLOC 16

int handle_query_response(SqlStatement *stmt, int32_t cursor_id, void *_parms, char *plan_name, boolean_t send_row_description) {
	QueryResponseParms *parms = (QueryResponseParms *)_parms;
	RowDescription *    row_description;
	RowDescriptionParm  row_desc_parm;
	RoctoSession *	    session = parms->session;

	ydb_buffer_t session_buffers[4], octo_buffers[3];
	ydb_buffer_t value_buffer, plan_name_buffer;

	SqlSetStatement * set_stmt;
	SqlShowStatement *show_stmt;
	SqlValue *	  runtime_variable, *runtime_value;
	int		  status, result = 0;
	DataRow *	  data_row;
	DataRowParm *	  data_row_parms;

	// A NULL stmt means the query was canceled and we should not generate DataRows
	if (NULL != stmt) {
		if (set_STATEMENT == stmt->type) {
			// SET a runtime variable to a specified value by updating the appropriate session LVN
			UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
			UNPACK_SQL_STATEMENT(runtime_value, set_stmt->value, value);
			UNPACK_SQL_STATEMENT(runtime_variable, set_stmt->variable, value);
			// Initialize session LVN subscripts
			YDB_STRING_TO_BUFFER(config->global_names.session, &session_buffers[0]);
			YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &session_buffers[1]);
			YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &session_buffers[2]);
			YDB_STRING_TO_BUFFER(runtime_variable->v.string_literal, &session_buffers[3]);
			YDB_STRING_TO_BUFFER(runtime_value->v.string_literal, &value_buffer);
			assert(value_buffer.len_used < YDB_MAX_STR); // No known PostgreSQL variables exceed YDB_MAX_STR
			status = ydb_set_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
			if (YDB_OK != status)
				return 1;
			return 0;
		}
		if (show_STATEMENT == stmt->type) {
			int32_t data_row_parms_alloc_len;

			data_row_parms_alloc_len = DATA_ROW_PARMS_ARRAY_INIT_ALLOC * sizeof(DataRowParm);
			assert(0 < data_row_parms_alloc_len);
			data_row_parms = (DataRowParm *)malloc(data_row_parms_alloc_len);

			// Attempt to GET the value of the specified runtime variable from the appropriate session LVN
			UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
			UNPACK_SQL_STATEMENT(runtime_variable, show_stmt->variable, value);
			// Initialize session LVN subscripts
			YDB_STRING_TO_BUFFER(config->global_names.session, &session_buffers[0]);
			YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &session_buffers[1]);
			YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &session_buffers[2]);
			YDB_STRING_TO_BUFFER(runtime_variable->v.string_literal, &session_buffers[3]);
			YDB_MALLOC_BUFFER(&value_buffer, OCTO_INIT_BUFFER_LEN);
			value_buffer.len_alloc--; // Leave room for null terminator
			status = ydb_get_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
			// Expand value_buffer allocation until it's large enough to store the retrieved row value
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
				status = ydb_get_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
				assert(YDB_ERR_INVSTRLEN != status);
			}
			// If the variable isn't defined for the session, attempt to pull the value from the Octo GVN
			if (YDB_OK != status) {
				YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_buffers[0]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &octo_buffers[1]);
				YDB_STRING_TO_BUFFER(runtime_variable->v.string_literal, &octo_buffers[2]);
				status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
					status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				// If the variable isn't defined on the Octo GVN, the variable isn't defined at all.
				// In this case we will return an empty string.
				if (YDB_OK != status) {
					value_buffer.buf_addr[0] = '\0';
					value_buffer.len_used = 0;
				}
			}

			// Responses to Execute messages should not send RowDescriptions
			if (send_row_description) {
				// Send RowDescription
				memset(&row_desc_parm, 0, sizeof(RowDescriptionParm));
				row_desc_parm.name = value_buffer.buf_addr;
				row_description = make_row_description(&row_desc_parm, 1);
				send_message(session, (BaseMessage *)(&row_description->type));
				free(row_description);
			}

			// Send DataRow
			data_row_parms[0].value = value_buffer.buf_addr;
			data_row_parms[0].length = strlen(value_buffer.buf_addr);
			data_row_parms[0].format = TEXT_FORMAT;
			data_row = make_data_row(data_row_parms, 1, NULL);
			send_message(parms->session, (BaseMessage *)(&data_row->type));
			free(data_row);
			free(data_row_parms);
			parms->data_sent = TRUE;

			YDB_FREE_BUFFER(&value_buffer);
			return 0;
		}

		// Go through and make rows for each row in the output plan
		parms->data_sent = TRUE;

		// Responses to Execute messages should not send RowDescriptions
		if (send_row_description) {
			// Send RowDescription
			plan_name_buffer.buf_addr = plan_name;
			plan_name_buffer.len_alloc = plan_name_buffer.len_used = strnlen(plan_name, OCTO_PATH_MAX);
			row_description = get_plan_row_description(&plan_name_buffer);
			if (NULL == row_description) {
				return 1;
			}
			send_message(parms->session, (BaseMessage *)(&row_description->type));
			free(row_description);
		}

		// Send back row data
		result = send_result_rows(cursor_id, parms, plan_name);
	} else {
		// Issue ErrorResponse expected by clients indicating a CancelRequest was processed
		ERROR(ERR_ROCTO_QUERY_CANCELED, "");
	}
	if (NULL != memory_chunks) {
		// Memory chunks are no longer needed after the query has been processed, so free them here.
		OCTO_CFREE(memory_chunks);
	}
	return result;
}
