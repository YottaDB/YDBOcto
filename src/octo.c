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

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "lexer.h"

#define BUFFER_SIZE 1024

extern int yydebug;
FILE *yyin;

static int verbose_flag;
static int dry_run;
static int quiet;

int main(int argc, char **argv)
{
  int c, error = 0, i = 0, status;
  yyscan_t scanner;
  YY_BUFFER_STATE state;
  char buff[BUFFER_SIZE];
  int done;
  SqlStatement *result = 0;
  char *buffer;
  size_t buffer_size = 0;
  FILE *inputFile;
  FILE *out;
  ydb_buffer_t schema_global, table_name_buffer, table_create_buffer, null_buffer;

  inputFile = NULL;
  definedTables = NULL;
  table_name_buffer.buf_addr = malloc(BUFFER_SIZE);
  table_name_buffer.len_used = 0;
  table_name_buffer.len_alloc = BUFFER_SIZE;
  table_create_buffer.buf_addr = malloc(BUFFER_SIZE);
  table_create_buffer.len_used = 0;
  table_create_buffer.len_alloc = BUFFER_SIZE;

  /* This is needed for parsing table definition files */
  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return 1;
  }
  YDB_LITERAL_TO_BUFFER("^schema", &schema_global);
  YDB_LITERAL_TO_BUFFER("", &null_buffer);

  /* Parse input parameters */
  while (1)
  {
    static struct option long_options[] =
      {
        {"verbose", no_argument, &verbose_flag, 1},
        {"dry-run", no_argument, &dry_run, 1},
        {"quiet", no_argument, &dry_run, 1},
        {"input-file", required_argument, 0, 'f'},
        {"table-definition-file", required_argument, 0, 't'},
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
      verbose_flag = 1;
      break;
    case 'd':
      dry_run = 1;
      break;
    case 'f':
      assert(inputFile == NULL);
      inputFile = fopen(optarg, "r");
      if (inputFile == NULL)
      {
        fprintf(stderr, "Error opening input file %s\n", optarg);
        return 1;
      }
      break;
    case 't':
      inputFile = fopen(optarg, "r");
      assert(inputFile);
      yyin = inputFile;
      yy_switch_to_buffer(
            yy_create_buffer( yyin, YY_BUF_SIZE, scanner), scanner);
      if(yyparse(scanner, &result))
      {
        error = 1;
        fprintf(stderr, "Error parsing statement\n");
      }
      fclose(inputFile);
      inputFile = NULL;
      if(definedTables == NULL) {
        definedTables = result->v.table;
        dqinit(definedTables);
      } else {
        dqinsert(definedTables, result->v.table);
      }
      break;
    case 'q':
      quiet = 1;
      break;
    default:
      return 1;
    }
  }

  if(!getenv("ydb_dist")) {
    fprintf(stderr, "ydb_dist environment variable not set; please setup YottaDB\n");
    return 1;
  }

  /* Load the existing tables */
  do {
    status = ydb_subscript_next_s(&schema_global, 1, &table_name_buffer, &table_name_buffer);
    YDB_ASSERT(YDB_OK == status);
    if(table_name_buffer.len_used == 0)
      break;
    ydb_get_s(&schema_global, 1, &table_name_buffer, &table_create_buffer);
    table_create_buffer.buf_addr[table_create_buffer.len_used] = '\0';
    printf("Running command %s\n", table_create_buffer.buf_addr);
    state = yy_scan_string(table_create_buffer.buf_addr, scanner);
    if(yyparse(scanner, &result))
    {
      error = 1;
      fprintf(stderr, "Error parsing statement from database\n");
    }
    result = NULL;
  } while(1);

  yydebug = verbose_flag;
  if (inputFile == NULL)
    inputFile = stdin;

  do {
    if (!quiet)
      printf("OCTO> ");
    i = 0;
    while(!feof(inputFile))
    {
      assert(i < BUFFER_SIZE);
      c = fgetc(inputFile);
      if(c != -1)
        buff[i++] = c;
      if(c == ';')
        break;
    }
    buff[i] = '\0';
    if (!quiet)
      printf("Running SQL command %s\n", buff);
    state = yy_scan_string(buff, scanner);
    if(yyparse(scanner, &result))
    {
      error = 1;
      fprintf(stderr, "Error parsing statement\n");
    }
    if (!quiet)
      printf("Done!\n");
    if(dry_run)
      continue;
    if(result == 0)
      continue;
    switch(result->type)
    {
    case SELECT_STATEMENT:
      out = open_memstream(&buffer, &buffer_size);
      assert(out);
      emit_select_statement(out, result);
      fclose(out);
      if(!quiet)
        printf("%s\n", buffer);
      free(buffer);
      break;
    case TABLE_STATEMENT:
      out = open_memstream(&buffer, &buffer_size);
      assert(out);
      emit_create_table(out, result);
      fclose(out);
      if(!quiet)
        printf("%s\n", buffer);
      YDB_COPY_STRING_TO_BUFFER(result->v.table->tableName, &table_name_buffer, done)
      YDB_COPY_STRING_TO_BUFFER(buffer, &table_create_buffer, done)
      status = ydb_set_s(&schema_global, 1,
        &table_name_buffer,
        &table_create_buffer);
      free(buffer);
      break;
    case DROP_STATEMENT:
      YDB_COPY_STRING_TO_BUFFER(result->v.drop->table_name->v.value->v.column_reference, &table_name_buffer, done)
      status = ydb_delete_s(&schema_global, 1,
        &table_name_buffer,
        YDB_DEL_NODE);
      break;
    }
    free(result);
    result = 0;
  } while(!feof(inputFile));
  yy_delete_buffer(state, scanner);
  yylex_destroy(scanner);
  return error;
}
