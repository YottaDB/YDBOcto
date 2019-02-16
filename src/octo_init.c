/* Copyright (C) 2018 YottaDB, LLC
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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <sys/types.h>
#include <dirent.h>
#include <errno.h>

#include "octo.h"
#include "octo_types.h"

int octo_init() {
	int c, error = 0, status;
	ydb_buffer_t schema_global, table_name_buffer, table_create_buffer, null_buffer;
	ydb_buffer_t cursor_global, cursor_exe_global[3];
	ydb_buffer_t z_status, z_status_value;
	SqlStatement *result = 0;
	SqlTable *table, *t_table;
	DIR *dir;
	config = malloc(sizeof(OctoConfig));
	config->record_error_level = WARNING;
	config->dry_run = FALSE;
	config->tmp_dir = "./";
	err_buffer = stderr;

	// Verify that the directory exists, or issue an error
	dir = opendir(config->tmp_dir);
	if(dir == NULL) {
		FATAL(ERR_SYSCALL, "opendir (config.tmp_dir)", errno);
	}
	free(dir);

	definedTables = NULL;
	cur_input_max = MAX_STR_CONST;
	input_buffer_combined = malloc(MAX_STR_CONST);
	memset(input_buffer_combined, 0, MAX_STR_CONST);
	cur_input_index = 0;
	cur_input_more = NULL;
	eof_hit = 0;

	// Load existing tables
	table_name_buffer.buf_addr = malloc(MAX_STR_CONST);
	table_name_buffer.len_used = 0;
	table_name_buffer.len_alloc = MAX_STR_CONST;
	table_create_buffer.buf_addr = malloc(MAX_STR_CONST);
	table_create_buffer.len_used = 0;
	table_create_buffer.len_alloc = MAX_STR_CONST;

	YDB_LITERAL_TO_BUFFER("^schema", &schema_global);
	YDB_LITERAL_TO_BUFFER("", &null_buffer);
	do {
		status = ydb_subscript_next_s(&schema_global, 1, &table_name_buffer, &table_name_buffer);
		if(status == YDB_ERR_NODEEND) {
			break;
		}

		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		if(table_name_buffer.len_used == 0)
			break;
		ydb_get_s(&schema_global, 1, &table_name_buffer, &table_create_buffer);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		table_create_buffer.buf_addr[table_create_buffer.len_used] = '\0';
		INFO(CUSTOM_ERROR, "Running command %s\n", table_create_buffer.buf_addr);
		result = parse_line(table_create_buffer.buf_addr);
		if(result == NULL) {
			continue;
		}
		UNPACK_SQL_STATEMENT(table, result, table);
		if(definedTables == NULL) {
			definedTables = table;
			dqinit(definedTables);
		} else {
			dqinsert(definedTables, table, t_table);
		}
		free(result);
		result = NULL;
	} while(1);

	return 0;
}
