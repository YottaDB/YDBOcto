%{
/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "memory_chunk.h"

#define YY_INPUT(buf, result, max_size) while((result = get_input(buf, max_size)) == -1 && result != YY_NULL) { };

#define YY_USER_ACTION 							\
	yylloc->first_line = yylloc->last_line; 			\
	yylloc->first_column = yylloc->last_column;			\
	for(int i = 0; yytext[i] != '\0'; i++) { 			\
		if(yytext[i] == '\n') { 				\
			yylloc->last_line++; 				\
			yylloc->first_column = 1; 			\
			yylloc->last_column = 1; 			\
		}							\
		else {							\
			yylloc->last_column++;				\
		}							\
    }

/*
 The first set of tokens are auto converted; copy pasts from .y, and run
	s/%token \([A-Z_]\+\)/(?i:\1) { return \1; }/
 on the lines
*/

/* Copies from src to dst, incrementing both pointers as it goes
 * copied will be incremented by the number of characters copied
 */
#define COPY_STRING(dst, src, copied, max) while((copied) < (max) && (*src)) {*dst++ = *src++; (copied)++; }

#define YYSTYPE SqlStatement *

char string_buffer[MAX_STR_CONST];
char *str_buf_ptr;
int copied_so_far;

%}

%option stack
%option reentrant noyywrap never-interactive nounistd
%option bison-bridge
%option bison-locations
%option noyy_top_state
%option noyy_pop_state
%option noyy_push_state
%option noinput
%option nounput

%x STRING
%x SINGLE_STRING
%x IDENTIFIER
%x COMMENT
%x COMMENT_CSTYLE

%%

(?i:ADVANCE) { return ADVANCE; }
(?i:ALL) { return ALL; }
(?i:AND) { return AND; }
(?i:ANY) { return ANY; }
(?i:AS) { return AS; }
(?i:ASC) { return ASC; }
(?i:AVG) { return AVG; }
(?i:BEG) { return BEG; }
(?i:BETWEEN) { return BETWEEN; }
(?i:BY) { return BY; }
(?i:CASCADE) { return CASCADE; }
(?i:CASE) { return CASE; }
(?i:CHAR) { return CHAR; }
(?i:CHARACTER) { return CHARACTER; }
(?i:COLLATE) { return COLLATE; }
(?i:COMMAND) { return COMMAND; }
(?i:COMMIT) { return COMMIT; }
(?i:CORRESPONDING) { return CORRESPONDING; }
(?i:COUNT) { return COUNT; }
(?i:CREATE) { return CREATE; }
(?i:CROSS) { return CROSS; }
(?i:CURSOR) { return CURSOR; }
(?i:DEC) { return DEC; }
(?i:DECIMAL) { return DECIMAL; }
(?i:DEFAULT) { return DEFAULT; }
(?i:DELETE) { return DELETE; }
(?i:DELIM) { return DELIM; }
(?i:DESC) { return DESC; }
(?i:DISTINCT) { return DISTINCT; }
(?i:DROP) { return DROP; }
(?i:ELSE) { return ELSE; }
(?i:END) { return END; }
(?i:EXCEPT) { return EXCEPT; }
(?i:EXISTS) { return EXISTS; }
(?i:EXTRACT) { return EXTRACT; }
(?i:FROM) { return FROM; }
(?i:FULL) { return FULL; }
(?i:GLOBAL) { return GLOBAL; }
(?i:GROUP) { return GROUP; }
(?i:HAVING) { return HAVING; }
(?i:IDENTIFIER_START) { return IDENTIFIER_START; }
(?i:ILIKE) { return ILIKE; }
(?i:IN) { return IN; }
(?i:INNER) { return INNER; }
(?i:INSERT) { return INSERT; }
(?i:INT) { return INT; }
(?i:INTEGER) { return INTEGER; }
(?i:INTERSECT) { return INTERSECT; }
(?i:INTO) { return INTO; }
(?i:IS) { return IS; }
(?i:JOIN) { return JOIN; }
(?i:KEY) { return KEY; }
(?i:LEFT) { return LEFT; }
(?i:LIKE) { return LIKE; }
(?i:LIMIT) { return LIMIT; }
(?i:MAX) { return MAX; }
(?i:MIN) { return MIN; }
(?i:NATURAL) { return NATURAL; }
(?i:NOT) { return NOT; }
(?i:NUM) { return NUM; }
(?i:NUMERIC) { return NUMERIC; }
(?i:ON) { return ON; }
(?i:OR) { return OR; }
(?i:ORDER) { return ORDER; }
(?i:OUTER) { return OUTER; }
(?i:OVER) { return OVER; }
(?i:PACK) { return PACK; }
(?i:PARTITION) { return PARTITION; }
(?i:PIECE) { return PIECE; }
(?i:PRIMARY) { return PRIMARY; }
(?i:RESTRICT) { return RESTRICT; }
(?i:RIGHT) { return RIGHT; }
(?i:SELECT) { return SELECT; }
(?i:SET) { return SET; }
(?i:SIMILAR) { return SIMILAR; }
(?i:SHOW) { return SHOW; }
(?i:SMALLINT) { return SMALLINT; }
(?i:SOME) { return ANY; }			/* SOME is the same as ANY */
(?i:START) { return START; }
(?i:SUM) { return SUM; }
(?i:TABLE) { return TABLE; }
(?i:THEN) { return THEN; }
(?i:TO) { return TO; }
(?i:UNION) { return UNION; }
(?i:UNIQUE) { return UNIQUE; }
(?i:UNKNOWN) { return UNKNOWN; }
(?i:UNPACK) { return UNPACK; }
(?i:UPDATE) { return UPDATE; }
(?i:USING) { return USING; }
(?i:VALUES) { return VALUES; }
(?i:VARCHAR) { return VARCHAR; }
(?i:VARYING) { return VARYING; }
(?i:WHEN) { return WHEN; }
(?i:WHERE) { return WHERE; }
(?i:EXIT) { return EXIT; }
(?i:QUIT) { return QUIT; }

(?i:NULL) { return NULL_TOKEN; }
(?i:FALSE) { return FALSE_TOKEN; }
(?i:TRUE) { return TRUE_TOKEN; }
, { return COMMA; }
\( { return LEFT_PAREN; }
\) { return RIGHT_PAREN; }
; { return SEMICOLON; }
\+ { return PLUS; }
- { return MINUS; }
\* { return ASTERISK; }
\/ { return SOLIDUS; }
\% { return PERCENT; }
\. { return PERIOD; }
\= { return EQUALS; }
\<= { return LESS_THAN_OR_EQUALS; }
\>= { return GREATER_THAN_OR_EQUALS; }
\<\> { return NOT_EQUALS; }
\!\= { return NOT_EQUALS; }
\< { return LESS_THAN; }
\> { return GREATER_THAN; }
\| { return PIPE; }
\~ { return TILDE; }
\! { return EXCLAMATION; }
\[ { return LEFT_BRACKET; }
\] { return RIGHT_BRACKET; }
\: { return COLON; }
\\q { return QUIT; }

^[ \t\r\n]* { /* skip leading whitespace */
		if (0 == yylloc->first_line) {
			leading_spaces = strlen(yytext);
			old_input_index += leading_spaces;
			yylloc->last_column -= leading_spaces;
			yylloc->last_line = 0;
		}
	}

[0-9]*(\.)?[0-9]* {
                copied_so_far = 0;
                str_buf_ptr = string_buffer;
                char *yptr = yytext;
                COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST);
                *str_buf_ptr = '\0';
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = NUMERIC_LITERAL;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                strncpy((*yylval_param)->v.value->v.string_literal, string_buffer, copied_so_far+1);
                return LITERAL;
              }
\$\$[a-zA-Z0-9]*\^[a-zA-Z][a-zA-Z0-9]* {
                copied_so_far = 0;
                str_buf_ptr = string_buffer;
                char *yptr = yytext;
                COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST);
                *str_buf_ptr = '\0';
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                if(*string_buffer == '$') {
                  (*yylval_param)->v.value->type = PARAMETER_VALUE;
                  }
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
                char *c = string_buffer;
                while(*c != '\0') {
                  *d++ = toupper(*c++);
                }
                *d = '\0';
                return EXTRINSIC_FUNCTION;
	       }

[a-zA-Z$][a-zA-Z0-9_$^]* {
                copied_so_far = 0;
                str_buf_ptr = string_buffer;
                char *yptr = yytext;
                COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST);
                *str_buf_ptr = '\0';
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                if(*string_buffer == '$') {
                  (*yylval_param)->v.value->type = PARAMETER_VALUE;
                  }
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
                char *c = string_buffer;
                while(*c != '\0') {
                  *d++ = toupper(*c++);
                }
                *d = '\0';
                return IDENTIFIER_START;
              }
[a-zA-Z][a-zA-Z0-9_]*\.[a-zA-Z][a-zA-Z0-9_]* {
                copied_so_far = 0;
                str_buf_ptr = string_buffer;
                char *yptr = yytext;
                COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST);
                *str_buf_ptr = '\0';
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
                char *c = string_buffer;
                while(*c != '\0') {
                  *d++ = toupper(*c++);
                }
                *d = '\0';
                return IDENTIFIER_START;
              }
-- { BEGIN(COMMENT); }
# { BEGIN(COMMENT); }
"/*" { BEGIN(COMMENT_CSTYLE); }

[ \t\r\n]      ;  /* skip whitespace which is not part of a string */

\' { copied_so_far = 0; str_buf_ptr = string_buffer; BEGIN(SINGLE_STRING); }
<SINGLE_STRING>\'\' { char *yptr = yytext + 1; COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST); }
<SINGLE_STRING>[^\']* { char *yptr = yytext; COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST); }
<SINGLE_STRING>\' {
              *str_buf_ptr = '\0';
              SQL_STATEMENT(*yylval_param, value_STATEMENT);
              OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
              (*yylval_param)->v.value->type = STRING_LITERAL;
              (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
              strncpy((*yylval_param)->v.value->v.string_literal, string_buffer, copied_so_far+1);
              BEGIN(INITIAL);
              return LITERAL;
            }


\" { copied_so_far = 0; str_buf_ptr = string_buffer; BEGIN(STRING); }
<STRING>\"\" { char *yptr = yytext + 1; COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST); }
<STRING>[^\"]* { char *yptr = yytext; COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST); }
<STRING>\" {
              *str_buf_ptr = '\0';
              SQL_STATEMENT(*yylval_param, value_STATEMENT);
              OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
              (*yylval_param)->v.value->type = STRING_LITERAL;
              (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
              strncpy((*yylval_param)->v.value->v.string_literal, string_buffer, copied_so_far+1);
              BEGIN(INITIAL);
              return LITERAL;
            }

` { copied_so_far = 0; str_buf_ptr = string_buffer; BEGIN(IDENTIFIER); }
<IDENTIFIER>[^(``)]* { char *yptr = yytext; COPY_STRING(str_buf_ptr, yptr, copied_so_far, MAX_STR_CONST); }
<IDENTIFIER>` {
                BEGIN(INITIAL);
                *str_buf_ptr = '\0';
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
                char *c = string_buffer;
                while(*c != '\0') {
                  *d++ = toupper(*c++);
                }
                *d = '\0';
                BEGIN(INITIAL);
                return IDENTIFIER_START;
              }

<COMMENT>[\n\r] { BEGIN(INITIAL); }
<COMMENT>.* { /* Ignore comments */ }

<COMMENT_CSTYLE>"*/" { BEGIN(INITIAL); }
<COMMENT_CSTYLE>. { /* Ignore comments */ }

<<EOF>> { printf("\n"); eof_hit = TRUE; return ENDOFFILE; }


. { return INVALID_TOKEN; }

%%
