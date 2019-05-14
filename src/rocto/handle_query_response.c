/* Copyright (C) 2018-2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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


void handle_query_response(SqlStatement *stmt, PhysicalPlan *plan, int cursor_id, void *_parms, ydb_buffer_t *outputKeyId) {
	QueryResponseParms *parms = (QueryResponseParms*)_parms;
	RowDescription *row_description;
	CommandComplete *command_complete;
	DataRow *data_row;
	/// TODO: we should add a new constant to define the maxium number of rows
	DataRowParm data_row_parms[MAX_STR_CONST];
	RoctoSession *session = parms->session;
	// Large chunks copied from print_temporary table, mostly ydb_buffer stuff
	/// WARNING: the ordering of these buffers is essential to the ydb calls;
	//   if altered, make sure the order is correct
	ydb_buffer_t ydb_buffers[9];
	ydb_buffer_t *cursor_b = &ydb_buffers[0], *cursor_id_b = &ydb_buffers[1],
	*keys_b = &ydb_buffers[2], *key_id_b = &ydb_buffers[3],
	*space_b = &ydb_buffers[4], *space_b2 = &ydb_buffers[5],
	*row_id_b = &ydb_buffers[6], *row_value_b = &ydb_buffers[7],
	*empty_buffer = &ydb_buffers[8], *val_buff;
	ydb_buffer_t z_status, z_status_value;
	PhysicalPlan *deep_plan = plan;
	SqlSetStatement *set_stmt;
	SqlShowStatement *show_stmt;
	SqlValue *val1, *val2;
	RowDescription *description;
	RowDescriptionParm row_desc_parm;
	char buffer[MAX_STR_CONST], *c;
	int status, number_of_columns = 0, row_count;

	YDB_LITERAL_TO_BUFFER("", space_b);
	YDB_LITERAL_TO_BUFFER("", space_b2);
	INIT_YDB_BUFFER(row_id_b, MAX_STR_CONST);

	YDB_LITERAL_TO_BUFFER("", empty_buffer);
	INIT_YDB_BUFFER(row_value_b, MAX_STR_CONST);

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

	while(deep_plan->next != NULL) {
		deep_plan = deep_plan->next;
	}

	row_description = get_plan_row_description(deep_plan);
	send_message(parms->session, (BaseMessage*)(&row_description->type));
	free(row_description);

	YDB_STRING_TO_BUFFER(config->global_names.cursor, cursor_b);

	snprintf(buffer, MAX_STR_CONST, "%d", cursor_id);
	cursor_id_b->len_used = strlen(buffer);
	cursor_id_b->buf_addr = malloc(cursor_id_b->len_used);
	memcpy(cursor_id_b->buf_addr, buffer, cursor_id_b->len_used+1);
	cursor_id_b->len_alloc = cursor_id_b->len_used;

	YDB_LITERAL_TO_BUFFER("keys", keys_b);

	snprintf(buffer, MAX_STR_CONST, "%d", deep_plan->outputKey->random_id);
	key_id_b->len_used = strlen(buffer);
	key_id_b->buf_addr = malloc(key_id_b->len_used);
	memcpy(key_id_b->buf_addr, buffer, key_id_b->len_used+1);
	key_id_b->len_alloc = key_id_b->len_used;


	status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
	if(status == YDB_ERR_NODEEND) {
		return;
	}
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	row_count = 0;

	while(!YDB_BUFFER_IS_SAME(empty_buffer, row_id_b)) {
		row_count++;
		status = ydb_get_s(cursor_b, 6, cursor_id_b, row_value_b);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		row_value_b->buf_addr[row_value_b->len_used] = '\0';
		number_of_columns = 0;
		data_row_parms[number_of_columns].value = row_value_b->buf_addr;
		for(c = row_value_b->buf_addr; *c != '\0'; c++) {
			if(*c == '|') {
				data_row_parms[number_of_columns].length = c - data_row_parms[number_of_columns].value;
				number_of_columns++;
				data_row_parms[number_of_columns].value = c + 1;
			}
		}
		data_row_parms[number_of_columns].length = c - data_row_parms[number_of_columns].value;
		if(c != row_value_b->buf_addr)
			number_of_columns++;
		data_row = make_data_row(data_row_parms, number_of_columns);
		send_message(parms->session, (BaseMessage*)(&data_row->type));
		free(data_row);
		// Move to the next subscript
		status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
		if(status == YDB_ERR_NODEEND) {
			break;
		}
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	}
	free(cursor_id_b->buf_addr);
	free(key_id_b->buf_addr);
	free(row_id_b->buf_addr);
	free(row_value_b->buf_addr);

	snprintf(buffer, MAX_STR_CONST, "SELECT %d", row_count);
	command_complete = make_command_complete(buffer);
	send_message(parms->session, (BaseMessage*)(&command_complete->type));
	free(command_complete);
	return;
}
