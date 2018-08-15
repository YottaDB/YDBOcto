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

%option stack
%option yylineno

%%

(?i:CHAR) { return CHAR; }
(?i:CHARACTER) { return CHARACTER; }
(?i:COMMAND) { return COMMAND; }
(?i:CREATE) { return CREATE; }
(?i:DEC) { return DEC; }
(?i:DECIMAL) { return DECIMAL; }
(?i:INT) { return INT; }
(?i:INTEGER) { return INTEGER; }
(?i:NUMERIC) { return NUMERIC; }
(?i:SMALLINT) { return SMALLINT; }
(?i:TABLE) { return TABLE; }
(?i:VARCHAR) { return VARCHAR; }
(?i:VARYING) { return VARYING; }


, { return COMMA; }
\( { return LEFT_PAREN; }
\) { return RIGHT_PAREN; }
\; { return SEMICOLON; }

[0-9][0-9]* { return UNSIGNED_INTEGER; }
(?i:[a-zA-Z][a-zA-Z0-9_]) { return IDENTIFIER_START; }

[ \t] { }

%%
