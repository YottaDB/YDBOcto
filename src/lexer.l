%{
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
(?i:AIMTYPE) { return AIMTYPE; }
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
(?i:CHECK) { return CHECK; }
(?i:COALESCE) { return COALESCE; }
(?i:COLLATE) { return COLLATE; }
(?i:COMMAND) { return COMMAND; }
(?i:COMMIT) { return COMMIT; }
(?i:CONSTRAINT) { return CONSTRAINT; }
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
(?i:GENERATED[ \t\n]+ALWAYS[ \t\n]+AS[ \t\n]+IDENTITY) { return GENERATED_ALWAYS_AS_IDENTITY; }
(?i:GENERATED[ \t\n]+BY[ \t\n]+DEFAULT[ \t\n]+AS[ \t\n]+IDENTITY) { return GENERATED_BY_DEFAULT_AS_IDENTITY; }
(?i:GLOBAL) { return GLOBAL; }
(?i:GREATEST) { return GREATEST; }
(?i:GROUP) { return GROUP; }
(?i:HAVING) { return HAVING; }
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
(?i:NAME) { return NAME; }			/* A NAME is a VARCHAR representing an internal system name. Since Octo doesn't
						 * distinguish between "system" strings and regular strings, we can just treat this
						 * as a regular VARCHAR.
						 *
						 * For more background, see: https://www.postgresql.org/docs/9.6/datatype-character.html
						 */
(?i:NOT) { return NOT; }
(?i:NUM) { return NUM; }
(?i:NULLIF) { return NULLIF; }
(?i:NUMERIC) { return NUMERIC; }
(?i:ON) { return ON; }
(?i:OR) { return OR; }
(?i:ORDER) { return ORDER; }
(?i:OUTER) { return OUTER; }
(?i:OVER) { return OVER; }
(?i:OVERRIDING[ \t\n]+SYSTEM[ \t\n]+VALUE) { return OVERRIDING_SYSTEM_VALUE; }
(?i:OVERRIDING[ \t\n]+USER[ \t\n]+VALUE) { return OVERRIDING_USER_VALUE; }
(?i:PACK) { return PACK; }
(?i:PARTITION) { return PARTITION; }
(?i:PIECE) { return PIECE; }
(?i:PRIMARY) { return PRIMARY; }
(?i:READONLY) { return READONLY; }
(?i:READWRITE) { return READWRITE; }
(?i:REGCLASS) { return REGCLASS; }
(?i:REGPROC) { return REGPROC; }
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
(?i:TRUNCATE) { return TRUNCATE; }
(?i:UNION) { return UNION; }
(?i:UNIQUE) { return UNIQUE; }
(?i:UNKNOWN) { return UNKNOWN; }
(?i:UNPACK) { return UNPACK; }
(?i:UPDATE) { return UPDATE; }
(?i:USING) { return USING; }
(?i:VALUES) { return VALUES; }
(?i:VARCHAR) { return VARCHAR; }
(?i:VARYING) { return VARYING; }
(?i:VIEW) { return VIEW; }
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
\\d { return DISPLAY; }
\\dv { return DISPLAY_VIEW; }

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
		SQL_VALUE_MALLOC_STATEMENT(*yylval, NUMERIC_LITERAL, copied_so_far);
                strncpy((*yylval)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
		RESET_BUFFER(lex_buffer);
                return LITERAL;
              }
[0-9]* {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
		SQL_VALUE_MALLOC_STATEMENT(*yylval, INTEGER_LITERAL, copied_so_far);
                strncpy((*yylval)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
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
		SQL_VALUE_MALLOC_STATEMENT(*yylval, COLUMN_REFERENCE, copied_so_far);
                char *d = (*yylval)->v.value->v.string_literal;
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
		SQL_VALUE_MALLOC_STATEMENT(*yylval, PARAMETER_VALUE, copied_so_far);
                char *d = (*yylval)->v.value->v.string_literal;
		TOLOWER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return LITERAL;
              }
\$[a-zA-Z]+ {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
		SQL_VALUE_MALLOC_STATEMENT(*yylval, COLUMN_REFERENCE, copied_so_far);
                char *d = (*yylval)->v.value->v.string_literal;
		TOLOWER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return INTRINSIC_FUNCTION;
              }
(?i:current_catalog|current_role|current_user|current_schema|session_user|user|localtime|localtimestamp|current_time|current_timestamp) {
		/* These SQL functions are syntactically exceptional in that they must be called without parentheses.
		 * See the note at https://www.postgresql.org/docs/11/functions-info.html for details.
		 */
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
		SQL_VALUE_MALLOC_STATEMENT(*yylval, COLUMN_REFERENCE, copied_so_far);
                char *d = (*yylval)->v.value->v.string_literal;
		TOLOWER(d, d+copied_so_far+1, lex_buffer.buf_addr, &lex_buffer.buf_addr[lex_buffer.len_used]);
		RESET_BUFFER(lex_buffer);
                return PARENLESS_FUNCTION;
              }
[a-zA-Z_][a-zA-Z0-9_$^]* {
                copied_so_far = 0;
                char *yptr = yytext;
                COPY_STRING((&lex_buffer), yptr, copied_so_far);
		SQL_VALUE_MALLOC_STATEMENT(*yylval, COLUMN_REFERENCE, copied_so_far);
		strncpy((*yylval)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
		RESET_BUFFER(lex_buffer);
                return IDENTIFIER_ALONE;
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
			SQL_VALUE_MALLOC_STATEMENT(*yylval, STRING_LITERAL, copied_so_far);
			strncpy((*yylval)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
			/* Reset the start of the lexer token to the opening quote to allow correct syntax highlighting in case of
			 * error. If this step is omitted, then the syntax highlighting will only begin at the closing quote.
			 */
			yylloc->first_column = yylloc->last_column - copied_so_far - 2; // -2 for opening and closing quotes
			retval = LITERAL;
		} else {
			/* We saw no bytes in between the starting and ending single-quote.
			 * That is, it is an empty string. Treat it as if a NULL keyword was specified,
			 * but since "IS NULL" and "IS ''" are treated differently (former is allowed, but latter
			 * should issue a syntax error), return a different lexical token in this case than what
			 * would have been returned for NULL (i.e. NULL_TOKEN). The parser will take care of issuing
			 * an error for the "IS ''" case and otherwise merge these two tokens into one type (NUL_VALUE).
			 */
			retval = EMPTY_STRING;
		}
		RESET_BUFFER(lex_buffer);
		BEGIN(INITIAL);
		return retval;
            }

\" { copied_so_far = 0; BEGIN(STRING); }
<STRING>\"\" { char *yptr = yytext + 1; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<STRING>[^\"]* { char *yptr = yytext; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<STRING>\" {

		SQL_VALUE_MALLOC_STATEMENT(*yylval, STRING_LITERAL, copied_so_far);
		strncpy((*yylval)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
		/* Double quoted literals may be identifiers or string literals, depending on context.
		 * In either case, they are case sensitive, so signal that here.
		 */
		(*yylval)->v.value->is_double_quoted = TRUE;
		RESET_BUFFER(lex_buffer);
		BEGIN(INITIAL);
		/* Reset the start of the lexer token to the opening quote to allow correct syntax highlighting in case of
		 * error. If this step is omitted, then the syntax highlighting will only begin at the closing quote.
		 */
		yylloc->first_column = yylloc->last_column - copied_so_far - 2; // -2 for opening and closing quotes
		return DOUBLE_QUOTE_LITERAL;
            }

` { copied_so_far = 0; BEGIN(IDENTIFIER); }
<IDENTIFIER>[^`]* { char *yptr = yytext; COPY_STRING((&lex_buffer), yptr, copied_so_far); }
<IDENTIFIER>` {
		BEGIN(INITIAL);
		SQL_VALUE_MALLOC_STATEMENT(*yylval, COLUMN_REFERENCE, copied_so_far);
		strncpy((*yylval)->v.value->v.string_literal, lex_buffer.buf_addr, copied_so_far+1);
                BEGIN(INITIAL);
		/* Reset the start of the lexer token to the opening quote to allow correct syntax highlighting in case of
		 * error. If this step is omitted, then the syntax highlighting will only begin at the closing quote.
		 */
		yylloc->first_column = yylloc->last_column - copied_so_far - 2; // -2 for opening and closing quotes
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
