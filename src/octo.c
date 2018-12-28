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
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "octo.h"
#include "octo_types.h"
#include "physical_plan.h"
#include "parser.h"
#include "lexer.h"

extern int yydebug;

int main(int argc, char **argv)
{
	int c, error = 0, status;
	int done;
	char *buffer;
	size_t buffer_size = 0;
	FILE *inputFile;
	FILE *out;
	SqlValue *value;
	SqlTable *table, *t_table;
	SqlStatement *result = 0;
	SqlStatement *tmp_statement;
	ydb_buffer_t schema_global, table_name_buffer, table_create_buffer, null_buffer;
	ydb_buffer_t cursor_global, cursor_exe_global[3];
	ydb_buffer_t z_status, z_status_value;
	gtm_char_t err_msgbuf[MAX_STR_CONST];
	gtm_long_t cursorId;
	PhysicalPlan *pplan;

	octo_init();

	inputFile = NULL;

	/* Parse input parameters */
	while (1)
	{
		static struct option long_options[] =
		{
			{"verbose", optional_argument, NULL, 'v'},
			{"dry-run", no_argument, NULL, 'd'},
			{"input-file", required_argument, NULL, 'f'},
			{0, 0, 0, 0}
		};
		int option_index = 0;

		c = getopt_long(argc, argv, "vdf:t:", long_options, &option_index);
		if(c == -1)
			break;

		switch(c)
		{
		case 0:
			if(long_options[option_index].flag != 0)
				break;
			break;
		case 'v':
			if(optarg) {
				c = atoi(optarg);
				if(c > FATAL || c < TRACE) {
					ERROR(CUSTOM_ERROR, "Invalid value specified for --verbose");
					return 1;
				}
				config->record_error_level = FATAL - c;
			} else {
				config->record_error_level = config->record_error_level > TRACE
				                             ? config->record_error_level - 1 : config->record_error_level;
			}
			break;
		case 'd':
			config->dry_run = 1;
			break;
		case 'f':
			assert(inputFile == NULL);
			inputFile = fopen(optarg, "r");
			if (inputFile == NULL)
			{
				FATAL(ERR_FILE_NOT_FOUND, optarg);
			}
			break;
		default:
			ERROR(CUSTOM_ERROR, "Uknown argument");
			return 1;
		}
	}
	table_name_buffer.buf_addr = malloc(MAX_STR_CONST);
	table_name_buffer.len_used = 0;
	table_name_buffer.len_alloc = MAX_STR_CONST;
	table_create_buffer.buf_addr = malloc(MAX_STR_CONST);
	table_create_buffer.len_used = 0;
	table_create_buffer.len_alloc = MAX_STR_CONST;

	YDB_LITERAL_TO_BUFFER("^schema", &schema_global);
	YDB_LITERAL_TO_BUFFER("", &null_buffer);
	YDB_LITERAL_TO_BUFFER("^cursor", &cursor_global);
	INIT_YDB_BUFFER(&cursor_exe_global[0], MAX_STR_CONST);
	YDB_LITERAL_TO_BUFFER("exe", &cursor_exe_global[1]);
	INIT_YDB_BUFFER(&cursor_exe_global[2], MAX_STR_CONST);

	TRACE(CUSTOM_ERROR, "Octo started");

	/* Load the existing tables */
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

	yydebug = config->record_error_level == TRACE && FALSE;
	if (inputFile == NULL)
		inputFile = stdin;

	do {
		if(readline_getc(inputFile, input_buffer_combined, MAX_STR_CONST) == -1)
			break;
		INFO(CUSTOM_ERROR, "Parsing SQL command %s", input_buffer_combined);
		result = parse_line(input_buffer_combined);
		INFO(CUSTOM_ERROR, "Done!");
		if(result == NULL)
			continue;
		if(config->dry_run) {
			cleanup_sql_statement(result);
			result = NULL;
			continue;
		}
		status = ydb_incr_s(&schema_global, 0, NULL, NULL, &cursor_exe_global[0]);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		cursor_exe_global[0].buf_addr[cursor_exe_global[0].len_used] = '\0';
		cursor_exe_global[2].len_used = 1;
		*cursor_exe_global[2].buf_addr = '0';
		switch(result->type)
		{
		case select_STATEMENT:
			pplan = emit_select_statement(&cursor_global, cursor_exe_global, result, NULL);
			if(pplan == NULL)
				break;
			cursorId = atol(cursor_exe_global[0].buf_addr);
			status = gtm_ci("select", cursorId);
			if (status != 0)
			{
				gtm_zstatus(err_msgbuf, MAX_STR_CONST);
				FATAL(ERR_YOTTADB, err_msgbuf);
			}
			print_temporary_table(pplan, cursorId);
			//cleanup_sql_statement(result);
			break;
		case table_STATEMENT:
			out = open_memstream(&buffer, &buffer_size);
			assert(out);
			emit_create_table(out, result);
			fclose(out);
			INFO(CUSTOM_ERROR, "%s", buffer);
			//buffer2 = m_escape_string(buffer);
			UNPACK_SQL_STATEMENT(value, result->v.table->tableName, value);
			YDB_COPY_STRING_TO_BUFFER(value->v.reference, &table_name_buffer, done)
			YDB_COPY_STRING_TO_BUFFER(buffer, &table_create_buffer, done)
			status = ydb_set_s(&schema_global, 1,
			                   &table_name_buffer,
			                   &table_create_buffer);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);
			free(buffer);
			if(definedTables == NULL) {
				definedTables = result->v.table;
				dqinit(definedTables);
			} else {
				dqinsert(definedTables, result->v.table, t_table);
			}
			free(result);
			break;
		case drop_STATEMENT:
			YDB_COPY_STRING_TO_BUFFER(result->v.drop->table_name->v.value->v.reference, &table_name_buffer, done)
			status = ydb_delete_s(&schema_global, 1,
			                      &table_name_buffer,
			                      YDB_DEL_NODE);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);
			cleanup_sql_statement(result);
			break;
		case insert_STATEMENT:
			pplan = emit_insert_statement(&cursor_global, cursor_exe_global, result);
			if(pplan == NULL)
				break;
			cursorId = atol(cursor_exe_global[0].buf_addr);
			status = gtm_ci("select", cursorId);
			if (status != 0)
			{
				gtm_zstatus(err_msgbuf, MAX_STR_CONST);
				FATAL(ERR_YOTTADB, err_msgbuf);
			}
			print_temporary_table(pplan, cursorId);
			//cleanup_sql_statement(result);
			break;
		default:
			FATAL(ERR_FEATURE_NOT_IMPLEMENTED, input_buffer_combined);
			break;
		}
		result = NULL;
	} while(!feof(inputFile));
	free(table_name_buffer.buf_addr);
	free(table_create_buffer.buf_addr);
	if(definedTables != NULL) {
		SQL_STATEMENT(tmp_statement, table_STATEMENT);
		tmp_statement->v.table = definedTables;
		cleanup_sql_statement(tmp_statement);
	}
	return error;
}
