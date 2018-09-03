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
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#include "parser.h"

void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, char const *s)
{
  int cur_line = 1, cur_column = 1, i = 0;
  char *expr_begin, *expr_end, t, *c;
  for(c = input_buffer_combined; *c != '\0' && cur_line != llocp->first_line; c++) {
    if(*c == '\n')
      cur_line++;
    assert(++i < MAX_STR_CONST);
  }
  expr_begin = c;
  printf("Error with syntax near (line %d, column %d):\n%s\n", llocp->first_line, llocp->first_column, expr_begin);
  for(; *c != '\0' && cur_column != llocp->first_column; c++, cur_column++) {
    assert(++i < MAX_STR_CONST);
    printf(" ");
  }
  for(; *c != '\0' && cur_line != llocp->last_line; c++) {
    if(*c == '\n')
      cur_line++;
  }
  for(; *c != '\0' && cur_column != llocp->last_column; c++, cur_column++) {
    printf("^");
    assert(++i < MAX_STR_CONST);
  }
  t = *c;
  *c = '\0';
  *c = t;
  printf("\n%s\n", s);
}
