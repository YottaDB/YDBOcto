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

#include "octo.h"
#include "parser.h"
#include "lexer.h"

#define BUFFER_SIZE 1024

extern int yydebug;
FILE *yyin;

static int verbose_flag;
static int dry_run;

int main(int argc, char **argv)
{
  int c, error = 0, i = 0;
  yyscan_t scanner;
  YY_BUFFER_STATE state;
  char buff[BUFFER_SIZE];
  FILE *inputFile;

  inputFile = NULL;
  /* Parse input parameters */
  while (1)
  {
    static struct option long_options[] =
      {
        {"verbose", no_argument, &verbose_flag, 1},
        {"dry-run", no_argument, &dry_run, 1},
        {"input-file", required_argument, 0, 'f'},
        {0, 0, 0, 0}
      };
    int option_index = 0;

    c = getopt_long(argc, argv, "vdf:", long_options, &option_index);
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
    default:
      return 1;
    }
  }
  yydebug = verbose_flag;

  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return 1;
  }
  if (inputFile == NULL)
    inputFile = stdin;

  do {
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
    printf("Running SQL command %s\n", buff);
    state = yy_scan_string(buff, scanner);
    if(yyparse(scanner))
    {
      error = 1;
      fprintf(stderr, "Error parsing statement\n");
    }
    printf("Done!\n");
    if(dry_run)
      continue;
  } while(!feof(inputFile));
  yy_delete_buffer(state, scanner);
  yylex_destroy(scanner);
  return error;
}
