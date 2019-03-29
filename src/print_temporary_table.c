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
void print_temporary_table(PhysicalPlan *plan, int cursor_id, void *parms) {
	char buffer[MAX_STR_CONST];
	/// WARNING: the ordering of these buffers is essential to the ydb calls;
	//   if altered, make sure the order is correct
	ydb_buffer_t ydb_buffers[9];
	ydb_buffer_t *cursor_b = &ydb_buffers[0], *cursor_id_b = &ydb_buffers[1],
		*keys_b = &ydb_buffers[2], *key_id_b = &ydb_buffers[3],
		*space_b = &ydb_buffers[4], *space_b2 = &ydb_buffers[5],
		*row_id_b = &ydb_buffers[6], *row_value_b = &ydb_buffers[7],
		*empty_buffer = &ydb_buffers[8];
	ydb_buffer_t z_status, z_status_value;
	PhysicalPlan *deep_plan = plan;
	int status;

	INFO(CUSTOM_ERROR, "%s", "print_temporary_table()");

	while(deep_plan->next != NULL) {
		deep_plan = deep_plan->next;
	}

	YDB_STRING_TO_BUFFER(config->global_names.cursor, cursor_b);

	snprintf(buffer, MAX_STR_CONST, "%d", cursor_id);
	cursor_id_b->len_used = strlen(buffer);
	cursor_id_b->buf_addr = malloc(cursor_id_b->len_used + 1);
	memcpy(cursor_id_b->buf_addr, buffer, cursor_id_b->len_used+1);
	cursor_id_b->len_alloc = cursor_id_b->len_used;

	YDB_LITERAL_TO_BUFFER("keys", keys_b);

	snprintf(buffer, MAX_STR_CONST, "%d", deep_plan->outputKey->random_id);
	key_id_b->len_used = strlen(buffer);
	key_id_b->buf_addr = malloc(key_id_b->len_used + 1);
	memcpy(key_id_b->buf_addr, buffer, key_id_b->len_used+1);
	key_id_b->len_alloc = key_id_b->len_used;

	YDB_LITERAL_TO_BUFFER(" ", space_b);
	YDB_LITERAL_TO_BUFFER(" ", space_b2);
	INIT_YDB_BUFFER(row_id_b, MAX_STR_CONST);

	YDB_LITERAL_TO_BUFFER("", empty_buffer);
	INIT_YDB_BUFFER(row_value_b, MAX_STR_CONST);

	status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
	if(status == YDB_ERR_NODEEND) {
		return;
	}
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	while(!YDB_BUFFER_IS_SAME(empty_buffer, row_id_b)) {
		status = ydb_get_s(cursor_b, 6, cursor_id_b, row_value_b);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		row_value_b->buf_addr[row_value_b->len_used] = '\0';
		fprintf(stdout, "%s\n", row_value_b->buf_addr);
		status = ydb_subscript_next_s(cursor_b, 6, cursor_id_b, row_id_b);
		if(status == YDB_ERR_NODEEND) {
			break;
		}
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	}
	fflush(stdout);
	free(cursor_id_b->buf_addr);
	free(key_id_b->buf_addr);
	free(row_id_b->buf_addr);
	free(row_value_b->buf_addr);
	return;
}
