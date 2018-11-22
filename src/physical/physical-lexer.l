%{
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
#include <ctype.h>

#include "physical-parser.h"

#define COPY_STRING(dst, src, copied, max) while((copied) < (max) && (*src)) {*dst++ = *src++; (copied)++; }

#define MAX_STR_CONST 32768
char string_buffer[MAX_STR_CONST];
char *str_buf_ptr;
int copied_so_far;

%}

%option stack
%option bison-bridge
%option noyywrap

%x VALUE_STATE
%x EXPR_STATE
%x EXPR_STATE_END

%%

"{%" { BEGIN(EXPR_STATE);
       str_buf_ptr = string_buffer;
       copied_so_far = 0;
       return START_EXPR;
     }
<EXPR_STATE>"%}" {
          BEGIN(EXPR_STATE_END);
          (*yylval_param) = (Expr*)malloc(sizeof(Expr));
          (*yylval_param)->type = EXPR_TYPE;
          (*yylval_param)->value = malloc(copied_so_far + 1);
          strncpy((*yylval_param)->value, string_buffer, copied_so_far);
          yyless(0);
          return LITERAL;
}
<EXPR_STATE>. { *str_buf_ptr++ = *yytext; copied_so_far++; }
<EXPR_STATE>\n { *str_buf_ptr++ = *yytext; copied_so_far++; }

<EXPR_STATE_END>"%}" { BEGIN(INITIAL); return END_EXPR; }

\{\{ { BEGIN(VALUE_STATE); return START_VALUE; }
<VALUE_STATE>\}\} { BEGIN(INITIAL); return END_VALUE; }
<VALUE_STATE>[^\}]* {
   copied_so_far = 0;
   str_buf_ptr = string_buffer;
   char *yptr = yytext;
   COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST);
   (*yylval_param) = (Expr*)malloc(sizeof(Expr));
   (*yylval_param)->type = VALUE_TYPE;
   (*yylval_param)->value = malloc(copied_so_far + 1);
   strncpy((*yylval_param)->value, string_buffer, copied_so_far);
   return LITERAL;
   }

[^\{]* {
   copied_so_far = 0;
   str_buf_ptr = string_buffer;
   char *yptr = yytext;
   COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST);
   (*yylval_param) = (Expr*)malloc(sizeof(Expr));
   (*yylval_param)->type = LITERAL_TYPE;
   (*yylval_param)->value = malloc(copied_so_far + 1);
   strncpy((*yylval_param)->value, string_buffer, copied_so_far);
   return LITERAL;
   }

<<EOF>> { return ENDOFFILE; }
