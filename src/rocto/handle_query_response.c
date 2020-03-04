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

#define	DATA_ROW_PARMS_ARRAY_INIT_ALLOC	16

int handle_query_response(SqlStatement *stmt, int32_t cursor_id, void *_parms, char *plan_name, boolean_t send_row_description) {
	QueryResponseParms	*parms = (QueryResponseParms*)_parms;
	RowDescription		*row_description;
	RowDescriptionParm	row_desc_parm;
	RoctoSession		*session = parms->session;

	ydb_buffer_t		*session_buffers, *octo_buffers;
	ydb_buffer_t		value_buffer, plan_name_buffer;

	SqlSetStatement		*set_stmt;
	SqlShowStatement		*show_stmt;
	SqlValue		*runtime_variable, *runtime_value;
	int			status, result = 0;
	DataRow			*data_row;
	DataRowParm		*data_row_parms;
	int32_t			data_row_parms_alloc_len = 0, done;

	// A NULL stmt means the query was canceled and we should not generate DataRows
	if (NULL != stmt) {
		YDB_MALLOC_BUFFER(&value_buffer, MAX_STR_CONST);
		if (set_STATEMENT == stmt->type) {
			// SET a runtime variable to a specified value by updating the appropriate session LVN
			UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
			UNPACK_SQL_STATEMENT(runtime_value, set_stmt->value, value);
			UNPACK_SQL_STATEMENT(runtime_variable, set_stmt->variable, value);
			session_buffers = make_buffers(config->global_names.session, 3, session->session_id->buf_addr, "variables", runtime_variable->v.string_literal);
			YDB_COPY_STRING_TO_BUFFER(runtime_value->v.string_literal, &value_buffer, done);
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
		data_row_parms_alloc_len = DATA_ROW_PARMS_ARRAY_INIT_ALLOC * sizeof(DataRowParm);
		data_row_parms = (DataRowParm*)malloc(data_row_parms_alloc_len);
		if (show_STATEMENT == stmt->type) {
			// Attempt to GET the value of the specified runtime variable from the appropriate session LVN
			UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
			UNPACK_SQL_STATEMENT(runtime_variable, show_stmt->variable, value);
			session_buffers = make_buffers(config->global_names.session, 3, session->session_id->buf_addr, "variables",
					runtime_variable->v.string_literal);
			status = ydb_get_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
			// If the variable isn't defined for the session, attempt to pull the value from the Octo GVN
			if (YDB_OK != status) {
				octo_buffers = make_buffers(config->global_names.octo, 2, "variables", runtime_variable->v.string_literal);
				status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
				// If the variable isn't defined on the Octo GVN, the variable isn't defined at all.
				// In this case we will return an empty string.
				if (YDB_OK != status) {
					value_buffer.buf_addr[0] = '\0';
					value_buffer.len_used = 0;
				}
				free(octo_buffers);
			}

			// Responses to Execute messages should not send RowDescriptions
			if (send_row_description) {
				// Send RowDescription
				memset(&row_desc_parm, 0, sizeof(RowDescriptionParm));
				row_desc_parm.name = value_buffer.buf_addr;
				row_description = make_row_description(&row_desc_parm, 1);
				send_message(session, (BaseMessage*)(&row_description->type));
				free(row_description);
			}

			// Send DataRow
			assert(0 < data_row_parms_alloc_len);
			data_row_parms[0].value = value_buffer.buf_addr;
			data_row_parms[0].length = strlen(value_buffer.buf_addr);
			data_row = make_data_row(data_row_parms, 1);
			send_message(parms->session, (BaseMessage*)(&data_row->type));
			free(data_row);
			free(data_row_parms);
			parms->data_sent = TRUE;

			YDB_FREE_BUFFER(&value_buffer);
			free(session_buffers);
			return 0;
		}

		// Go through and make rows for each row in the output plan
		parms->data_sent = TRUE;

		// Responses to Execute messages should not send RowDescriptions
		if(send_row_description) {
			// Send RowDescription
			plan_name_buffer.buf_addr = plan_name;
			plan_name_buffer.len_alloc = plan_name_buffer.len_used = strnlen(plan_name, OCTO_PATH_MAX);
			row_description = get_plan_row_description(&plan_name_buffer);
			if (NULL == row_description) {
				YDB_FREE_BUFFER(&value_buffer);
				free(data_row_parms);
				return 1;
			}
			send_message(parms->session, (BaseMessage*)(&row_description->type));
			free(row_description);
		}

		// Send back row data
		result = send_result_rows(cursor_id, parms, plan_name);
	} else {
		// Issue ErrorResponse expected by clients indicating a CancelRequest was processed
		ERROR(ERR_ROCTO_QUERY_CANCELED, "");
		UNUSED(data_row_parms_alloc_len);
	}
	if(memory_chunks != NULL) {
		OCTO_CFREE(memory_chunks);
	}
	return result;
}
