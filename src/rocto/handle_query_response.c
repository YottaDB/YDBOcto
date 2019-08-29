/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"
#include "physical_plan.h"
#include "helpers.h"

#define	DATA_ROW_PARMS_ARRAY_INIT_ALLOC	16

void handle_query_response(SqlStatement *stmt, int32_t cursor_id, void *_parms, char *plan_name) {
	QueryResponseParms	*parms = (QueryResponseParms*)_parms;
	RowDescription		*row_description;
	CommandComplete		*command_complete;
	RoctoSession		*session = parms->session;
	// Large chunks copied from print_temporary table, mostly ydb_buffer stuff
	/// WARNING: the ordering of these buffers is essential to the ydb calls;
	//   if altered, make sure the order is correct
	ydb_buffer_t		ydb_buffers[9];
	ydb_buffer_t		plan_name_b, *outputKeyId;
	ydb_buffer_t		*cursor_b = &ydb_buffers[0], *cursor_id_b = &ydb_buffers[1],
				*keys_b = &ydb_buffers[2], *key_id_b = &ydb_buffers[3],
				*space_b = &ydb_buffers[4], *space_b2 = &ydb_buffers[5],
				*row_id_b = &ydb_buffers[6], *row_value_b = &ydb_buffers[7],
				*empty_buffer = &ydb_buffers[8], *val_buff;
	SqlSetStatement		*set_stmt;
	SqlShowStatement	*show_stmt;
	SqlValue		*val1, *val2;
	RowDescription		*description;
	RowDescriptionParm	row_desc_parm;
	char			buffer[MAX_STR_CONST], *c;
	int			status, number_of_columns = 0, row_count;
	DataRow			*data_row;
	static DataRowParm	*data_row_parms = NULL;
	static int		data_row_parms_alloc_len = 0;

	YDB_LITERAL_TO_BUFFER("", space_b);
	YDB_LITERAL_TO_BUFFER("", space_b2);
	YDB_MALLOC_BUFFER(row_id_b, MAX_STR_CONST);

	YDB_LITERAL_TO_BUFFER("", empty_buffer);
	YDB_MALLOC_BUFFER(row_value_b, MAX_STR_CONST);

	session->session_id->buf_addr[session->session_id->len_used] = '\0';

	if(stmt->type == set_STATEMENT) {
		UNPACK_SQL_STATEMENT(set_stmt, stmt, set);
		UNPACK_SQL_STATEMENT(val1, set_stmt->value, value);
		UNPACK_SQL_STATEMENT(val2, set_stmt->variable, value);
		set(val1->v.string_literal, config->global_names.session, 3,
				session->session_id->buf_addr, "variables", val2->v.string_literal);
		snprintf(buffer, MAX_STR_CONST, "SET");
		command_complete = make_command_complete(buffer);
		send_message(parms->session, (BaseMessage*)(&command_complete->type));
		free(command_complete);
		return;
	}
	if (NULL == data_row_parms)
	{
		data_row_parms = malloc(DATA_ROW_PARMS_ARRAY_INIT_ALLOC * sizeof(DataRowParm));
		data_row_parms_alloc_len = DATA_ROW_PARMS_ARRAY_INIT_ALLOC;
	}
	if(stmt->type == show_STATEMENT) {
		UNPACK_SQL_STATEMENT(show_stmt, stmt, show);
		UNPACK_SQL_STATEMENT(val1, show_stmt->variable, value);
		val_buff = get(config->global_names.session, 3, session->session_id->buf_addr, "variables",
				val1->v.string_literal);
		if(val_buff == NULL) {
			val_buff = get(config->global_names.octo, 2, session->session_id->buf_addr,
					"variables", val1->v.string_literal);
		}
		if(val_buff == NULL) {
			val_buff = empty_buffer;
		}
		// Send RowDescription
		memset(&row_desc_parm, 0, sizeof(RowDescriptionParm));
		row_desc_parm.name = val1->v.string_literal;
		description = make_row_description(&row_desc_parm, 1);
		send_message(session, (BaseMessage*)(&description->type));
		free(description);

		// Send DataRow
		assert(0 < data_row_parms_alloc_len);
		data_row_parms[0].value = val_buff->buf_addr;
		data_row_parms[0].length = strlen(val_buff->buf_addr);
		data_row = make_data_row(data_row_parms, 1);
		send_message(parms->session, (BaseMessage*)(&data_row->type));
		free(data_row);
		parms->data_sent = TRUE;

		// Done
		snprintf(buffer, MAX_STR_CONST, "SHOW");
		command_complete = make_command_complete(buffer);
		send_message(parms->session, (BaseMessage*)(&command_complete->type));
		free(command_complete);
		return;
	}

	// Go through and make rows for each row in the output plan
	parms->data_sent = TRUE;

	plan_name_b.buf_addr = plan_name;
	plan_name_b.len_alloc = plan_name_b.len_used = strlen(plan_name);
	row_description = get_plan_row_description(&plan_name_b);
	send_message(parms->session, (BaseMessage*)(&row_description->type));
	free(row_description);

	YDB_STRING_TO_BUFFER(config->global_names.cursor, cursor_b);

	snprintf(buffer, MAX_STR_CONST, "%d", cursor_id);
	cursor_id_b->len_used = strlen(buffer);
	cursor_id_b->buf_addr = malloc(cursor_id_b->len_used + 1);
	memcpy(cursor_id_b->buf_addr, buffer, cursor_id_b->len_used+1);
	cursor_id_b->len_alloc = cursor_id_b->len_used;

	YDB_LITERAL_TO_BUFFER("keys", keys_b);

	outputKeyId = get("^%ydboctoocto", 3, "plan_metadata", plan_name, "output_key");
	if(outputKeyId == NULL) {
		FATAL(ERR_DATABASE_FILES_OOS, "");
		return;
	}
	*key_id_b = *outputKeyId;

	status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
	if(status == YDB_ERR_NODEEND) {
		return;
	}
	YDB_ERROR_CHECK(status);
	row_count = 0;

	if (parms->max_data_to_send <= 0) {
		parms->max_data_to_send = INT32_MAX;
	}
	while(row_count < parms->max_data_to_send) {
		row_count++;
		status = ydb_get_s(cursor_b, 6, cursor_id_b, row_value_b);
		YDB_ERROR_CHECK(status);
		row_value_b->buf_addr[row_value_b->len_used] = '\0';
		number_of_columns = 0;
		assert(number_of_columns < data_row_parms_alloc_len);
		data_row_parms[number_of_columns].value = row_value_b->buf_addr;
		for(c = row_value_b->buf_addr; *c != '\0'; c++) {
			if(*c == '|') {
				assert(number_of_columns < data_row_parms_alloc_len);
				data_row_parms[number_of_columns].length = c - data_row_parms[number_of_columns].value;
				number_of_columns++;
				if (number_of_columns >= data_row_parms_alloc_len)
				{	/* Current allocation is not enough. Expand to twice current size. */
					DataRowParm	*tmp;

					tmp = malloc(data_row_parms_alloc_len * 2 * sizeof(DataRowParm));
					memcpy(tmp, data_row_parms, (data_row_parms_alloc_len * sizeof(DataRowParm)));
					free(data_row_parms);
					data_row_parms = tmp;
					data_row_parms_alloc_len = data_row_parms_alloc_len * 2;
				}
				assert(number_of_columns < data_row_parms_alloc_len);
				data_row_parms[number_of_columns].value = c + 1;
			}
		}
		data_row_parms[number_of_columns].length = c - data_row_parms[number_of_columns].value;
		assert(number_of_columns < data_row_parms_alloc_len);
		number_of_columns++;
		data_row = make_data_row(data_row_parms, number_of_columns);
		send_message(parms->session, (BaseMessage*)(&data_row->type));
		free(data_row);
		// Move to the next subscript
		status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
		if(status == YDB_ERR_NODEEND) {
			break;
		}
		YDB_ERROR_CHECK(status);
	}

	// Cleanup tables
	ydb_delete_s(cursor_b, 1, cursor_id_b, YDB_DEL_TREE);

	free(cursor_id_b->buf_addr);
	free(row_id_b->buf_addr);
	free(row_value_b->buf_addr);
	free(outputKeyId->buf_addr);
	free(outputKeyId);

	snprintf(buffer, MAX_STR_CONST, "SELECT %d", row_count);
	command_complete = make_command_complete(buffer);
	send_message(parms->session, (BaseMessage*)(&command_complete->type));
	free(command_complete);
	if(memory_chunks != NULL) {
		octo_cfree(memory_chunks);
		memory_chunks = NULL;
	}
	return;
}
