%{
/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "memory_chunk.h"
#include "memory_chunk.h"

#define YY_INPUT(buf, result, max_size)	result = get_input(buf, max_size)	/* reads 1 byte of input per invocation */

#define YY_USER_ACTION 							\
	yylloc->first_line = yylloc->last_line;				\
	yylloc->first_column = yylloc->last_column;			\
	for (int i = 0; yytext[i] != '\0'; i++) { 			\
		if (yytext[i] == '\n') { 				\
			yylloc->last_line++; 				\
			yylloc->first_column = 1; 			\
			yylloc->last_column = 1; 			\
		} else {						\
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
#define COPY_STRING(dst, src, copied)						\
	{									\
		size_t total_len;						\
		char * dst_str;							\
										\
		total_len = strlen(src);					\
		if (dst->len_alloc <= (copied + total_len)) {				\
			char *tmp;\
			int len_used;\
\
			tmp = (char*)malloc(sizeof(char) * (copied + total_len + 1));			\
			memcpy(tmp, dst->buf_addr, dst->len_used);\
			len_used = dst->len_used;\
			free(dst->buf_addr); \
			dst->len_alloc = copied + total_len + 1; \
			dst->buf_addr = tmp; \
			dst->len_used = len_used; \
		}								\
		dst_str =  dst->buf_addr + dst->len_used;	\
		while ((*src)) {							\
			*dst_str++ = *src++;					\
			(copied)++;						\
			dst->len_used++;					\
		}								\
		*dst_str = '\0';						\
	}

#define RESET_BUFFER(BUFFER) \
	BUFFER.len_used = 0; /* Reset the length of the buffer to indicate the value is no longer needed */ \
	BUFFER.buf_addr[0] = '\0'; /* Reset the length of the buffer to indicate the value is no longer needed */

#define YYSTYPE SqlStatement *

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
(?i:ARRAY) { return ARRAY; }
(?i:AS) { return AS; }
(?i:ASC) { return ASC; }
(?i:AVG) { return AVG; }
(?i:BEG) { return BEG; }
(?i:BETWEEN) { return BETWEEN; }
(?i:BIGINT) { return BIGINT; }
(?i:BOOL) { return BOOLEAN; }
(?i:BOOLEAN) { return BOOLEAN; }
(?i:BY) { return BY; }
(?i:CASCADE) { return CASCADE; }
(?i:CASE) { return CASE; }
(?i:CAST) { return CAST; }
(?i:CHAR) { return CHAR; }
(?i:CHARACTER) { return CHARACTER; }
(?i:COALESCE) { return COALESCE; }
(?i:COLLATE) { return COLLATE; }
(?i:COMMAND) { return COMMAND; }
(?i:COMMIT) { return COMMIT; }
(?i:CORRESPONDING) { return CORRESPONDING; }
(?i:COUNT) { return COUNT; }
(?i:CREATE) { return CREATE; }
(?i:CROSS) { return CROSS; }
(?i:DATE) { return DATE; }
(?i:DEC) { return DEC; }
(?i:DECIMAL) { return DECIMAL; }
(?i:DEFAULT) { return DEFAULT; }
(?i:DELETE) { return DELETE; }
(?i:DELIM) { return DELIM; }
(?i:DESC) { return DESC; }
(?i:DISCARD) { return DISCARD; }
(?i:DISTINCT) { return DISTINCT; }
(?i:DROP) { return DROP; }
(?i:ELSE) { return ELSE; }
(?i:END) { return END; }
(?i:ENDPOINT) { return ENDPOINT; }
(?i:EXCEPT) { return EXCEPT; }
(?i:EXISTS) { return EXISTS; }
(?i:EXTRACT) { return EXTRACT; }
(?i:FROM) { return FROM; }
(?i:FULL) { return FULL; }
(?i:FUNCTION) { return FUNCTION; }
(?i:GLOBAL) { return GLOBAL; }
(?i:GREATEST) { return GREATEST; }
(?i:GROUP) { return GROUP; }
(?i:HAVING) { return HAVING; }
(?i:IDENTIFIER_ALONE) { return IDENTIFIER_ALONE; }
(?i:IDENTIFIER_BACK_TICK) { return IDENTIFIER_BACK_TICK; }
(?i:IDENTIFIER_PERIOD_IDENTIFIER) { return IDENTIFIER_PERIOD_IDENTIFIER; }
(?i:IF) { return IF; }
(?i:ILIKE) { return ILIKE; }
(?i:IN) { return IN; }
(?i:INDEX) { return INDEX; }
(?i:INNER) { return INNER; }
(?i:INSERT) { return INSERT; }
(?i:INT) { return INT; }
(?i:INT2) { return INT2; }
(?i:INT4) { return INT4; }
(?i:INT8) { return INT8; }
(?i:INTEGER) { return INTEGER; }
(?i:INTERSECT) { return INTERSECT; }
(?i:INTO) { return INTO; }
(?i:IS) { return IS; }
(?i:JOIN) { return JOIN; }
(?i:KEEPDATA) { return KEEPDATA; }
(?i:KEY) { return KEY; }
(?i:LEAST) { return LEAST; }
(?i:LEFT) { return LEFT; }
(?i:LIKE) { return LIKE; }
(?i:LIMIT) { return LIMIT; }
(?i:MAX) { return MAX; }
(?i:MIN) { return MIN; }
(?i:NATURAL) { return NATURAL; }
(?i:NOT) { return NOT; }
(?i:NUM) { return NUM; }
(?i:NULLIF) { return NULLIF; }
(?i:NUMERIC) { return NUMERIC; }
(?i:ON) { return ON; }
(?i:OR) { return OR; }
(?i:ORDER) { return ORDER; }
(?i:OUTER) { return OUTER; }
(?i:OVER) { return OVER; }
(?i:PACK) { return PACK; }
(?i:PARENLESS_FUNCTION) { return PARENLESS_FUNCTION; }
(?i:PARTITION) { return PARTITION; }
(?i:PIECE) { return PIECE; }
(?i:PRIMARY) { return PRIMARY; }
(?i:READONLY) { return READONLY; }
(?i:READWRITE) { return READWRITE; }
(?i:RESTRICT) { return RESTRICT; }
(?i:RETURNS) { return RETURNS; }
(?i:RIGHT) { return RIGHT; }
(?i:SELECT) { return SELECT; }
(?i:SET) { return SET; }
(?i:SIMILAR) { return SIMILAR; }
(?i:SHOW) { return SHOW; }
(?i:SMALLINT) { return SMALLINT; }
(?i:SOME) { return ANY; }			/* SOME is the same as ANY */
(?i:START) { return START; }
(?i:STARTINCLUDE) { return STARTINCLUDE; }
(?i:SUM) { return SUM; }
(?i:TABLE) { return TABLE; }
(?i:TEXT) { return VARCHAR; }			/* TEXT is the same as VARCHAR */
(?i:THEN) { return THEN; }
(?i:TIME) { return TIME; }
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
\\s { return HISTORY; }

^[ \t\r\n]* { /* skip leading whitespace */
		if (0 == yylloc->first_line) {
			leading_spaces = strlen(yytext);
			old_input_index += leading_spaces;
			yylloc->last_column -= leading_spaces;
			yylloc->last_line = 0;
		}
	}

[0-9]*(\.)[0-9]* {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = NUMERIC_LITERAL;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                strncpy((*yylval_param)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
		RESET_BUFFER(lex_buffer);
                return LITERAL;
              }
[0-9]* {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = INTEGER_LITERAL;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                strncpy((*yylval_param)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
		RESET_BUFFER(lex_buffer);
                return LITERAL;
              }
\$\$[%a-zA-Z0-9]?[a-zA-Z0-9]*\^?[%a-zA-Z][a-zA-Z0-9]* {	/* M labels and M routine-names can have % as leading character.
							 * M labels can be all digits. M labels can be optional too. Hence this fancy regex rule.
							 * Note that this rule does not account an extra '%' in the label name, e.g. $$BAD%FUNC.
							 * This case is handled manually in the EXTRINSIC_FUNCTION rule in parser.y.
							 */
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
                char *c = lex_buffer.buf_addr;
                while(*c != '\0') {
                  *d++ = *c++;
                }
                *d = '\0';
		RESET_BUFFER(lex_buffer);
                return EXTRINSIC_FUNCTION;
	       }
\$[0-9]+ {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = PARAMETER_VALUE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
		TOUPPER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return LITERAL;
              }
\$[a-zA-Z]+ {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
		TOUPPER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return INTRINSIC_FUNCTION;
              }
(current_(catalog|role|user|schema))|((session_)?user) {
		/* The following SQL functions are syntactically exceptional
		* in that they must be called without parentheses:
		* current_catalog, current_role, current_user, current_schema, session_user, user.
		* See the note at https://www.postgresql.org/docs/11/functions-info.html for details.
		*/
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
		TOUPPER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return PARENLESS_FUNCTION;
              }
[a-zA-Z][a-zA-Z0-9_$^]* {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
		TOUPPER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return IDENTIFIER_ALONE;
              }
[a-zA-Z][a-zA-Z0-9_]*\.[a-zA-Z][a-zA-Z0-9_]* {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
		TOUPPER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return IDENTIFIER_PERIOD_IDENTIFIER;
              }
-- { BEGIN(COMMENT); }
# { BEGIN(COMMENT); }
"/*" { BEGIN(COMMENT_CSTYLE); }

[ \t\r\n]      ;  /* skip whitespace which is not part of a string */

\' { copied_so_far = 0; BEGIN(SINGLE_STRING); }
<SINGLE_STRING>\'\' { char *yptr = yytext + 1; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<SINGLE_STRING>[^\']* { char *yptr = yytext; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<SINGLE_STRING>\' {
		int	retval;

		if (copied_so_far) {
			/* We have read at least one byte between the starting and ending single-quote */
			SQL_STATEMENT(*yylval_param, value_STATEMENT);
			OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
			(*yylval_param)->v.value->type = STRING_LITERAL;
			(*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
			strncpy((*yylval_param)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
			retval = LITERAL;
		} else {
			/* We saw no bytes in between the starting and ending single-quote.
			 * That is, it is an empty string. Treat it as if a NULL keyword was specified.
			 */
			retval = NULL_TOKEN;
		}
		RESET_BUFFER(lex_buffer);
		BEGIN(INITIAL);
		return retval;
            }



\" { copied_so_far = 0; BEGIN(STRING); }
<STRING>\"\" { char *yptr = yytext + 1; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<STRING>[^\"]* { char *yptr = yytext; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<STRING>\" {
		SQL_STATEMENT(*yylval_param, value_STATEMENT);
		OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
		(*yylval_param)->v.value->type = STRING_LITERAL;
		(*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
		strncpy((*yylval_param)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
		RESET_BUFFER(lex_buffer);
		BEGIN(INITIAL);
		return LITERAL;
            }

` { copied_so_far = 0; BEGIN(IDENTIFIER); }
<IDENTIFIER>[^(``)]* { char *yptr = yytext; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<IDENTIFIER>` {
		BEGIN(INITIAL);
                SQL_STATEMENT(*yylval_param, value_STATEMENT);
                OCTO_CMALLOC_STRUCT((*yylval_param)->v.value, SqlValue);
                (*yylval_param)->v.value->type = COLUMN_REFERENCE;
                (*yylval_param)->v.value->v.string_literal = (char*)octo_cmalloc(memory_chunks, copied_so_far+1);
                char *d = (*yylval_param)->v.value->v.string_literal;
		TOUPPER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
                BEGIN(INITIAL);
		RESET_BUFFER(lex_buffer);
                return IDENTIFIER_BACK_TICK;
              }

<COMMENT>[\n\r] { BEGIN(INITIAL); }
<COMMENT>.* { /* Ignore comments */ }

<COMMENT_CSTYLE>"*/" { BEGIN(INITIAL); }
<COMMENT_CSTYLE>. { /* Ignore comments */ }
<COMMENT_CSTYLE>[\n\r] { /* Ignore multi-line C-style comments */ }

<<EOF>> { eof_hit = EOF_CTRLD; return ENDOFFILE; }


. { return INVALID_TOKEN; }

%%
