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
#include "parser.h"
#include "lexer.h"

extern int yydebug;

int main(int argc, char **argv)
{
  int c, error = 0, i = 0, status;
  yyscan_t scanner;
  YY_BUFFER_STATE state;
  int done;
  SqlStatement *result = 0;
  char *buffer;
  size_t buffer_size = 0;
  FILE *inputFile;
  FILE *out;
  SqlValue *value;
  SqlTable *table;
  SqlSelectStatement *select;
  SqlStatement *tmp_statement;
  ydb_buffer_t schema_global, table_name_buffer, table_create_buffer, null_buffer;
  ydb_buffer_t cursor_global, cursor_exe_global[2];
  ydb_buffer_t z_status, z_status_value;
  gtm_char_t      err_msgbuf[MAX_STR_CONST];

  config = malloc(sizeof(OctoConfig));
  config->record_error_level = WARNING;
  config->dry_run = FALSE;

  inputFile = NULL;
  definedTables = NULL;
  table_name_buffer.buf_addr = malloc(MAX_STR_CONST);
  table_name_buffer.len_used = 0;
  table_name_buffer.len_alloc = MAX_STR_CONST;
  table_create_buffer.buf_addr = malloc(MAX_STR_CONST);
  table_create_buffer.len_used = 0;
  table_create_buffer.len_alloc = MAX_STR_CONST;

  /* This is needed for parsing table definition files */
  if (yylex_init(&scanner))
    FATAL(ERR_INIT_SCANNER);
  YDB_LITERAL_TO_BUFFER("^schema", &schema_global);
  YDB_LITERAL_TO_BUFFER("", &null_buffer);
  YDB_LITERAL_TO_BUFFER("^cursor", &cursor_global);
  YDB_LITERAL_TO_BUFFER("0", &cursor_exe_global[0]);
  YDB_LITERAL_TO_BUFFER("exe", &cursor_exe_global[1]);

  /* Parse input parameters */
  while (1)
  {
    static struct option long_options[] =
      {
        {"verbose", no_argument, NULL, 'v'},
        {"dry-run", no_argument, NULL, 'd'},
        {"quiet", no_argument, NULL, 'q'},
        {"input-file", required_argument, NULL, 'f'},
        {0, 0, 0, 0}
      };
    int option_index = 0;

    c = getopt_long(argc, argv, "vdf:qt:", long_options, &option_index);
    if(c == -1)
      break;

    switch(c)
    {
    case 0:
      if(long_options[option_index].flag != 0)
        break;
      break;
    case 'v':
      config->record_error_level = config->record_error_level > TRACE
        ? config->record_error_level - 1 : config->record_error_level;
      break;
    case 'q':
      config->record_error_level = config->record_error_level < ERROR
        ? config->record_error_level + 1 : config->record_error_level;
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
      return 1;
    }
  }

  TRACE(CUSTOM_ERROR, "Octo started");

  /* Load the existing tables */
  do {
    status = ydb_subscript_next_s(&schema_global, 1, &table_name_buffer, &table_name_buffer);
    YDB_ERROR_CHECK(status, &z_status, &z_status_value);
    if(table_name_buffer.len_used == 0)
      break;
    ydb_get_s(&schema_global, 1, &table_name_buffer, &table_create_buffer);
    YDB_ERROR_CHECK(status, &z_status, &z_status_value);
    table_create_buffer.buf_addr[table_create_buffer.len_used] = '\0';
    INFO(CUSTOM_ERROR, "Running command %s\n", table_create_buffer.buf_addr);
    state = yy_scan_string(table_create_buffer.buf_addr, scanner);
    if(yyparse(scanner, &result))
    {
      /// TODO: we should emit a useful syntax error when this happens
      ERROR(ERR_PARSING_COMMAND, table_create_buffer.buf_addr);
      free(result);
      yy_delete_buffer(state, scanner);
      result = NULL;
      continue;
    }
    UNPACK_SQL_STATEMENT(table, result, table);
    if(definedTables == NULL) {
      definedTables = table;
      dqinit(definedTables);
    } else {
      dqinsert(definedTables, table);
    }
    free(result);
    yy_delete_buffer(state, scanner);
    result = NULL;
  } while(1);

  yydebug = config->record_error_level > INFO;
  if (inputFile == NULL)
    inputFile = stdin;

  do {
    if(readline_getc(inputFile, input_buffer_combined, MAX_STR_CONST) == -1)
      break;
    INFO(CUSTOM_ERROR, "Running SQL command %s", input_buffer_combined);
    state = yy_scan_string(input_buffer_combined, scanner);
    if(yyparse(scanner, &result))
    {
      error = 1;
      ERROR(ERR_PARSING_COMMAND, input_buffer_combined);
    }
    yy_delete_buffer(state, scanner);
    INFO(CUSTOM_ERROR, "Done!");
    if(result == 0)
      continue;
    if(config->dry_run) {
      cleanup_sql_statement(result);
      result = NULL;
      continue;
    }
    switch(result->type)
    {
    case select_STATEMENT:
      out = open_memstream(&buffer, &buffer_size);
      assert(out);
      emit_select_statement(out, result);
      fclose(out);
      INFO(CUSTOM_ERROR, "%s", buffer);
      YDB_COPY_STRING_TO_BUFFER(buffer, &table_name_buffer, done)
      status = ydb_set_s(&cursor_global, 2,
        cursor_exe_global,
        &table_name_buffer);
      YDB_ERROR_CHECK(status, &z_status, &z_status_value);
      status = gtm_ci("select");
       if (status != 0)
       {
                gtm_zstatus(err_msgbuf, MAX_STR_CONST);
                FATAL(ERR_YOTTADB, err_msgbuf);
       }
      free(buffer);
      cleanup_sql_statement(result);
      break;
    case table_STATEMENT:
      out = open_memstream(&buffer, &buffer_size);
      assert(out);
      emit_create_table(out, result);
      fclose(out);
      INFO(CUSTOM_ERROR, "%s", buffer);
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
        dqinsert(definedTables, result->v.table);
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
    }
    result = NULL;
  } while(!feof(inputFile));
  yylex_destroy(scanner);
  free(table_name_buffer.buf_addr);
  free(table_create_buffer.buf_addr);
  if(definedTables != NULL) {
    SQL_STATEMENT(tmp_statement, table_STATEMENT);
    tmp_statement->v.table = definedTables;
    cleanup_sql_statement(tmp_statement);
  }
  return error;
}
