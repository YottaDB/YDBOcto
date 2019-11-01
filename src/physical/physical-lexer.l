/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

%{
/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

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
%option noyy_top_state
%option noyy_pop_state
%option noyy_push_state
%option noinput
%option nounput

%x VALUE_STATE
%x EXPR_STATE
%x EXPR_STATE_END

%%

"{}%" { BEGIN(EXPR_STATE);
       str_buf_ptr = string_buffer;
       copied_so_far = 0;
       return START_EXPR;
     }
<EXPR_STATE>"%{}" {
          BEGIN(EXPR_STATE_END);
          (*yylval_param) = (Expr*)malloc(sizeof(Expr));
          (*yylval_param)->type = EXPR_TYPE;
          (*yylval_param)->value = malloc(copied_so_far + 1);
          strncpy((*yylval_param)->value, string_buffer, copied_so_far);
          (*yylval_param)->value[copied_so_far] = '\0';
          yyless(0);
          return LITERAL;
}
<EXPR_STATE>. { *str_buf_ptr++ = *yytext; copied_so_far++; }
<EXPR_STATE>\n { *str_buf_ptr++ = *yytext; copied_so_far++; }

<EXPR_STATE_END>"%{}" { BEGIN(INITIAL); return END_EXPR; }

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
   (*yylval_param)->value[copied_so_far] = '\0';
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
   (*yylval_param)->value[copied_so_far] = '\0';
   return LITERAL;
   }

<<EOF>> { return ENDOFFILE; }
