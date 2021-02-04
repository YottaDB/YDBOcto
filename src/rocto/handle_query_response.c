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

int handle_query_response(SqlStatement *stmt, ydb_long_t cursorId, void *_parms, char *plan_name, PSQL_MessageTypeT msg_type) {
	ydb_buffer_t session_buffers[4], octo_buffers[3];
	ydb_buffer_t value_buffer, plan_name_buffer;
	int	     status, result = 0;

	/* A NULL stmt means the query was canceled and we should not process the query response (e.g. generate DataRows) */
	if (NULL != stmt) {
		QueryResponseParms *parms;
		RoctoSession *	    session;

		parms = (QueryResponseParms *)_parms;
		session = parms->session;
		if (set_STATEMENT == stmt->type) {
			/* SET the current value of a runtime variable by looking at the appropriate session LVN */
			if (PSQL_Query == msg_type) {
				/* Caller is "handle_query()" (simple query protocol). Do SET of runtime variable right away */
				SqlValue *	 runtime_variable, *runtime_value;
				SqlSetStatement *set_stmt;

				// Initialize session LVN subscripts
				UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
				UNPACK_SQL_STATEMENT(runtime_value, set_stmt->value, value);
				UNPACK_SQL_STATEMENT(runtime_variable, set_stmt->variable, value);
				YDB_STRING_TO_BUFFER(config->global_names.session, &session_buffers[0]);
				YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &session_buffers[1]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &session_buffers[2]);
				YDB_STRING_TO_BUFFER(runtime_variable->v.string_literal, &session_buffers[3]);
				YDB_STRING_TO_BUFFER(runtime_value->v.string_literal, &value_buffer);
				assert(value_buffer.len_used < YDB_MAX_STR); // No known PostgreSQL variables exceed YDB_MAX_STR
				status = ydb_set_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
				if (YDB_OK != status) {
					return 1;
				}
			} else if (PSQL_Parse == msg_type) {
				/* Caller is "handle_parse()" (extended query protocol). Set up prepared statement lvns for
				 * later use in "handle_bind()" followed by "handle_execute()" (to do SET of runtime variable).
				 */
				ydb_buffer_t	 statement_subs[5];
				SqlValue *	 runtime_variable, *runtime_value;
				SqlSetStatement *set_stmt;

				UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
				UNPACK_SQL_STATEMENT(runtime_value, set_stmt->value, value);
				UNPACK_SQL_STATEMENT(runtime_variable, set_stmt->variable, value);
				YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
				YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
				YDB_STRING_TO_BUFFER(OCTOLIT_PREPARED, &statement_subs[2]);
				YDB_STRING_TO_BUFFER(parms->parse_dest, &statement_subs[3]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLE, &statement_subs[4]);
				YDB_STRING_TO_BUFFER(runtime_variable->v.string_literal, &value_buffer);
				status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &value_buffer);
				if (YDB_OK != status) {
					return 1;
				}
				YDB_STRING_TO_BUFFER(OCTOLIT_VALUE, &statement_subs[4]);
				YDB_STRING_TO_BUFFER(runtime_value->v.string_literal, &value_buffer);
				status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &value_buffer);
				if (YDB_OK != status) {
					return 1;
				}
			} else {
				/* Caller is "handle_execute()" (extended query protocol). SET the runtime variable using the
				 * portal lvn nodes set up in the corresponding "handle_bind()" call (which used the prepared
				 * statement lvn nodes set up in the corresponding "handle_parse()" call of this function above).
				 */
				ydb_buffer_t statement_subs[5];
				ydb_buffer_t name_buffer;

				assert(PSQL_Execute == msg_type);
				YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
				YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
				YDB_STRING_TO_BUFFER(OCTOLIT_BOUND, &statement_subs[2]);
				YDB_STRING_TO_BUFFER(parms->portal_name, &statement_subs[3]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLE, &statement_subs[4]);
				YDB_MALLOC_BUFFER(&name_buffer, OCTO_INIT_BUFFER_LEN);
				status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &name_buffer);
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(name_buffer);
					status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &name_buffer);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					YDB_FREE_BUFFER(&name_buffer);
					return 1;
				}
				YDB_STRING_TO_BUFFER(OCTOLIT_VALUE, &statement_subs[4]);
				YDB_MALLOC_BUFFER(&value_buffer, OCTO_INIT_BUFFER_LEN);
				status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &value_buffer);
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
					status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &value_buffer);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					YDB_FREE_BUFFER(&name_buffer);
					YDB_FREE_BUFFER(&value_buffer);
					return 1;
				}
				YDB_STRING_TO_BUFFER(config->global_names.session, &session_buffers[0]);
				YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &session_buffers[1]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &session_buffers[2]);
				session_buffers[3] = name_buffer;
				assert(value_buffer.len_used < YDB_MAX_STR); // No known PostgreSQL variables exceed YDB_MAX_STR
				status = ydb_set_s(&session_buffers[0], 3, &session_buffers[1], &value_buffer);
				if (YDB_OK != status) {
					YDB_FREE_BUFFER(&name_buffer);
					YDB_FREE_BUFFER(&value_buffer);
					return 1;
				}
				YDB_FREE_BUFFER(&name_buffer);
				YDB_FREE_BUFFER(&value_buffer);
			}
			return 0;
		}
		if (show_STATEMENT == stmt->type) {
			/* SHOW the current value of the specified runtime variable by looking at the appropriate session LVN */
			ydb_buffer_t name_buffer;
			DataRowParm  data_row_parms;
			DataRow *    data_row;

			if (PSQL_Parse == msg_type) {
				/* Caller is "handle_parse()" (extended query protocol). Set up prepared statement lvns for
				 * later use in "handle_bind()" followed by "handle_execute()" (to do SHOW of runtime variable).
				 */
				ydb_buffer_t	  statement_subs[5];
				SqlValue *	  runtime_variable;
				SqlShowStatement *show_stmt;

				UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
				UNPACK_SQL_STATEMENT(runtime_variable, show_stmt->variable, value);
				YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
				YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
				YDB_STRING_TO_BUFFER(OCTOLIT_PREPARED, &statement_subs[2]);
				YDB_STRING_TO_BUFFER(parms->parse_dest, &statement_subs[3]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLE, &statement_subs[4]);
				YDB_STRING_TO_BUFFER(runtime_variable->v.string_literal, &value_buffer);
				status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &value_buffer);
				if (YDB_OK != status) {
					return 1;
				}
				return 0;
			}
			if (PSQL_Execute == msg_type) {
				/* Caller is "handle_execute()" (extended query protocol). SHOW the runtime variable using the
				 * portal lvn nodes set up in the corresponding "handle_bind()" call (which used the prepared
				 * statement lvn nodes set up in the corresponding "handle_parse()" call of this function above).
				 */
				ydb_buffer_t statement_subs[5];

				assert(PSQL_Execute == msg_type);
				YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
				YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
				YDB_STRING_TO_BUFFER(OCTOLIT_BOUND, &statement_subs[2]);
				YDB_STRING_TO_BUFFER(parms->portal_name, &statement_subs[3]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLE, &statement_subs[4]);
				YDB_MALLOC_BUFFER(&name_buffer, OCTO_INIT_BUFFER_LEN);
				status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &name_buffer);
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(name_buffer);
					status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &name_buffer);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					YDB_FREE_BUFFER(&name_buffer);
					return 1;
				}
				session_buffers[3] = name_buffer;
			} else {
				/* Caller is "handle_query()" (simple query protocol). Do the SHOW right away */
				SqlValue *	  runtime_variable;
				SqlShowStatement *show_stmt;

				assert(PSQL_Query == msg_type);
				UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
				UNPACK_SQL_STATEMENT(runtime_variable, show_stmt->variable, value);
				YDB_STRING_TO_BUFFER(runtime_variable->v.string_literal, &session_buffers[3]);
			}
			// Initialize session LVN subscripts
			YDB_STRING_TO_BUFFER(config->global_names.session, &session_buffers[0]);
			YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &session_buffers[1]);
			YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &session_buffers[2]);
			/* session_buffers[3] is already initialized to the appropriate value */

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
			if (YDB_ERR_LVUNDEF == status) {
				YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_buffers[0]);
				YDB_STRING_TO_BUFFER(OCTOLIT_VARIABLES, &octo_buffers[1]);
				octo_buffers[2] = session_buffers[3];
				status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
				if (YDB_ERR_INVSTRLEN == status) {
					EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
					status = ydb_get_s(&octo_buffers[0], 2, &octo_buffers[1], &value_buffer);
					assert(YDB_ERR_INVSTRLEN != status);
				}
				// If the variable isn't defined on the Octo GVN, the variable isn't defined at all.
				// In this case we will return an empty string.
				if (YDB_ERR_GVUNDEF == status) {
					status = YDB_OK;
					value_buffer.buf_addr[0] = '\0';
					value_buffer.len_used = 0;
				}
			}
			if (PSQL_Execute == msg_type) {
				YDB_FREE_BUFFER(&name_buffer);
			}
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				YDB_FREE_BUFFER(&value_buffer);
				return 1;
			}
			/* Send DataRow */
			data_row_parms.value = value_buffer.buf_addr;
			data_row_parms.length = value_buffer.len_used;
			data_row_parms.format = TEXT_FORMAT;
			data_row = make_data_row(&data_row_parms, 1, NULL);
			send_message(parms->session, (BaseMessage *)(&data_row->type));
			free(data_row);
			YDB_FREE_BUFFER(&value_buffer);
			parms->data_sent = TRUE;
			return 0;
		}
		parms->data_sent = TRUE; /* Note: In case of INSERT INTO etc. we do not send data rows but set this
					  * variable to TRUE even in that case as one caller function "handle_execute"
					  * relies on this.
					  */
		if (select_STATEMENT == stmt->type) {
			// Go through and make rows for each row in the output plan
			// Responses to Execute messages should not send RowDescriptions but Query messages should
			if (PSQL_Query == msg_type) {
				RowDescription *row_description;

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
			result = send_result_rows(cursorId, parms, plan_name); /* Note: updates parms.row_count */
		} else if (insert_STATEMENT == stmt->type) {
			/* It is of type INSERT INTO, DELETE FROM etc. In that case, there are no rows to send back.
			 * Just update "parms->row_count" to reflect the number of rows inserted, deleted etc..
			 */
			parms->row_count = get_row_count_from_plan_name(plan_name, cursorId);
		}
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
