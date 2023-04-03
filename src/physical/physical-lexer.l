/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

#define COPY_STRING(dst, src, copied, max)                                                                  \
	while ((copied) < (max) && (*src)) {                                                                    \
		*dst++ = *src++;                                                                                    \
		if (max > copied_so_far) {                                                                          \
			(copied)++;                                                                                     \
		} else {                                                                                            \
			fprintf(stderr, "FATAL: physical-lexer.l: BUFFER_SIZE %d insufficient to lex template\n", max); \
			exit(1);                                                                                        \
		}                                                                                                   \
	}

/* physical-lexer.l is used during compilation for generating C files from .ctemplate files and not at Octo runtime,
 * so the string_buffer below will be released prior to Octo runtime. The upper bound for what can be stored in
 * this buffer is fixed for a given commit of Octo and is equivalent to the largest M template file. So far, 32Kib
 * has been sufficient for all current M templates and will likely be sufficient for the foreseeable future.
 *
 * Accordingly, it is acceptable to use a fixed size of 32KiB. If an M template exceeds this limit, then the size of
 * this buffer will need to be increased.
 */
#define BUFFER_SIZE 32768
char string_buffer[BUFFER_SIZE];
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
          (*yylval) = (Expr*)malloc(sizeof(Expr));
          (*yylval)->type = EXPR_TYPE;
          (*yylval)->value = malloc(copied_so_far + 1);
          strncpy((*yylval)->value, string_buffer, copied_so_far);
          (*yylval)->value[copied_so_far] = '\0';
          yyless(0);
          return LITERAL;
}
<EXPR_STATE>. {
	*str_buf_ptr++ = *yytext;
	if (BUFFER_SIZE > copied_so_far) {
		copied_so_far++;
	} else {
		fprintf(stderr, "FATAL: physical-lexer.l: BUFFER_SIZE %d insufficient to lex template\n", BUFFER_SIZE);
		exit(1);
	}
}
<EXPR_STATE>\n { *str_buf_ptr++ = *yytext; copied_so_far++; }

<EXPR_STATE_END>"%{}" { BEGIN(INITIAL); return END_EXPR; }

\{\{ { BEGIN(VALUE_STATE); return START_VALUE; }
<VALUE_STATE>\}\} { BEGIN(INITIAL); return END_VALUE; }
<VALUE_STATE>[^\}]* {
   copied_so_far = 0;
   str_buf_ptr = string_buffer;
   char *yptr = yytext;
   COPY_STRING(str_buf_ptr, yptr, copied_so_far, BUFFER_SIZE);
   (*yylval) = (Expr*)malloc(sizeof(Expr));
   (*yylval)->type = VALUE_TYPE;
   (*yylval)->value = malloc(copied_so_far + 1);
   strncpy((*yylval)->value, string_buffer, copied_so_far);
   (*yylval)->value[copied_so_far] = '\0';
   return LITERAL;
   }

[^\{]* {
   copied_so_far = 0;
   str_buf_ptr = string_buffer;
   char *yptr = yytext;
   COPY_STRING(str_buf_ptr, yptr, copied_so_far, BUFFER_SIZE);
   (*yylval) = (Expr*)malloc(sizeof(Expr));
   (*yylval)->type = LITERAL_TYPE;
   (*yylval)->value = malloc(copied_so_far + 1);
   strncpy((*yylval)->value, string_buffer, copied_so_far);
   (*yylval)->value[copied_so_far] = '\0';
   return LITERAL;
   }

<<EOF>> { return ENDOFFILE; }
