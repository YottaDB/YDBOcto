#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"

#include "physical_plan.h"

/**
 * Iterates over the last output of the plan and prints it to the screen
 */
void print_temporary_table(PhysicalPlan *plan, int cursor_id) {
	char buffer[MAX_STR_CONST];
	/// WARNING: the ordering of these buffers is essential to the ydb calls;
	//   if altered, make sure the order is correct
	ydb_buffer_t cursor_b, cursor_id_b, keys_b, key_id_b, space_b, space_b2, row_id_b;
	ydb_buffer_t empty_buffer, row_value_b;
	ydb_buffer_t z_status, z_status_value;
	PhysicalPlan *deep_plan = plan;
	int status;

	while(deep_plan->next != NULL) {
		deep_plan = deep_plan->next;
	}

	YDB_LITERAL_TO_BUFFER("^cursor", &cursor_b);

	snprintf(buffer, MAX_STR_CONST, "%d", cursor_id);
	cursor_id_b.len_used = strlen(buffer);
	cursor_id_b.buf_addr = malloc(cursor_id_b.len_used);
	memcpy(cursor_id_b.buf_addr, buffer, cursor_id_b.len_used+1);
	cursor_id_b.len_alloc = cursor_id_b.len_used;

	YDB_LITERAL_TO_BUFFER("keys", &keys_b);

	snprintf(buffer, MAX_STR_CONST, "%d", deep_plan->outputKey->random_id);
	key_id_b.len_used = strlen(buffer);
	key_id_b.buf_addr = malloc(key_id_b.len_used);
	memcpy(key_id_b.buf_addr, buffer, key_id_b.len_used+1);
	key_id_b.len_alloc = key_id_b.len_used;

	YDB_LITERAL_TO_BUFFER(" ", &space_b);
	YDB_LITERAL_TO_BUFFER(" ", &space_b2);
	INIT_YDB_BUFFER(&row_id_b, MAX_STR_CONST);

	YDB_LITERAL_TO_BUFFER("", &empty_buffer);
	INIT_YDB_BUFFER(&row_value_b, MAX_STR_CONST);

	status = ydb_subscript_next_s(&cursor_b, 6, &cursor_id_b, &row_id_b);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	while(!YDB_BUFFER_IS_SAME(&empty_buffer, &row_id_b)) {
		status = ydb_get_s(&cursor_b, 6, &cursor_id_b, &row_value_b);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		row_value_b.buf_addr[row_value_b.len_used] = '\0';
		fprintf(stdout, "%s\n", row_value_b.buf_addr);
		status = ydb_subscript_next_s(&cursor_b, 6, &cursor_id_b, &row_id_b);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	}
	free(cursor_id_b.buf_addr);
	free(key_id_b.buf_addr);
	free(row_id_b.buf_addr);
	free(row_value_b.buf_addr);
	return;
	/*
	int status = 0, done = 0;
	char *first_char;
	SqlValue *tableName;
	ydb_buffer_t schema_global, rowid, row_value, empty_buffer;
	ydb_buffer_t z_status, z_status_value;

	INIT_YDB_BUFFER(&schema_global, MAX_STR_CONST);
	UNPACK_SQL_STATEMENT(tableName, table->tableName, value);
	*schema_global.buf_addr++ = '^';
	YDB_COPY_STRING_TO_BUFFER(tableName->v.reference, &schema_global, done);
	schema_global.buf_addr--;
	schema_global.len_used++;
	INIT_YDB_BUFFER(&rowid, MAX_STR_CONST);
	INIT_YDB_BUFFER(&row_value, MAX_STR_CONST);
	YDB_LITERAL_TO_BUFFER("", &empty_buffer);

	status = ydb_subscript_next_s(&schema_global, 1, &rowid, &rowid);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	while(!YDB_BUFFER_IS_SAME(&empty_buffer, &rowid)) {
		status = ydb_get_s(&schema_global, 1, &rowid, &row_value);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		row_value.buf_addr[row_value.len_used] = '\0';
		first_char = row_value.buf_addr;
		while(first_char < row_value.buf_addr + row_value.len_used && *first_char++ != '|') {
			// Intentionally left blank
		}
		fprintf(stdout, "|%s\n", first_char);
		status = ydb_subscript_next_s(&schema_global, 1, &rowid, &rowid);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	}

	free(row_value.buf_addr);*/
}
