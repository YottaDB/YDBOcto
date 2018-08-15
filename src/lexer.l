%{

#include <stdio.h>
#include <ctype.h>

#include "parser.h"

/*
 The first set of tokens are auto converted; copy pasts from .y, and run
	s/%token \([A-Z_]\+\)/(?i:\1) { return \1; }/
 on the lines
*/

%}

%option outfile="../build/lexer.c" header-file="../build/lexer.h"
%option stack
%option yylineno
%option reentrant noyywrap never-interactive nounistd
%option bison-bridge

%%

(?i:CHAR) { return CHAR; }
(?i:CHARACTER) { return CHARACTER; }
(?i:COMMAND) { return COMMAND; }
(?i:CREATE) { return CREATE; }
(?i:DEC) { return DEC; }
(?i:DECIMAL) { return DECIMAL; }
(?i:IDENTIFIER_START) { return IDENTIFIER_START; }
(?i:INT) { return INT; }
(?i:INTEGER) { return INTEGER; }
(?i:KEY) { return KEY; }
(?i:NOT) { return NOT; }
(?i:NUMERIC) { return NUMERIC; }
(?i:PRIMARY) { return PRIMARY; }
(?i:SMALLINT) { return SMALLINT; }
(?i:TABLE) { return TABLE; }
(?i:UNIQUE) { return UNIQUE; }
(?i:VARCHAR) { return VARCHAR; }
(?i:VARYING) { return VARYING; }

(?i:NULL) { return NULL_TOKEN; }
, { return COMMA; }
\( { return LEFT_PAREN; }
\) { return RIGHT_PAREN; }
\; { return SEMICOLON; }

[0-9][0-9]* { return UNSIGNED_INTEGER; }
[a-zA-Z][a-zA-Z0-9_]* { return IDENTIFIER_START; }

[ \t\r\n]      ;  /* skip whitespace which is not part of a string */

<<EOF>> { return ENDOFFILE; }

%%
