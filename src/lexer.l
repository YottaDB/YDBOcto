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

#include "parser.h"

/*
 The first set of tokens are auto converted; copy pasts from .y, and run
	s/%token \([A-Z_]\+\)/(?i:\1) { return \1; }/
 on the lines
*/

%}

%option stack
%option yylineno
%option reentrant noyywrap never-interactive nounistd
%option bison-bridge

%x STRING
%x IDENTIFIER

%%

(?i:ALL) { return ALL; }
(?i:AND) { return AND; }
(?i:AS) { return AS; }
(?i:ASC) { return ASC; }
(?i:AVG) { return AVG; }
(?i:BY) { return BY; }
(?i:CHAR) { return CHAR; }
(?i:CHARACTER) { return CHARACTER; }
(?i:COLLATE) { return COLLATE; }
(?i:COMMAND) { return COMMAND; }
(?i:CORRESPONDING) { return CORRESPONDING; }
(?i:COUNT) { return COUNT; }
(?i:CREATE) { return CREATE; }
(?i:CROSS) { return CROSS; }
(?i:DEC) { return DEC; }
(?i:DECIMAL) { return DECIMAL; }
(?i:DEFAULT) { return DEFAULT; }
(?i:DELETE) { return DELETE; }
(?i:DESC) { return DESC; }
(?i:DISTINCT) { return DISTINCT; }
(?i:EXCEPT) { return EXCEPT; }
(?i:FALSE) { return FALSE; }
(?i:FROM) { return FROM; }
(?i:FULL) { return FULL; }
(?i:GROUP) { return GROUP; }
(?i:HAVING) { return HAVING; }
(?i:IDENTIFIER_START) { return IDENTIFIER_START; }
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
(?i:MAX) { return MAX; }
(?i:MIN) { return MIN; }
(?i:NATURAL) { return NATURAL; }
(?i:NOT) { return NOT; }
(?i:NUMERIC) { return NUMERIC; }
(?i:ON) { return ON; }
(?i:OR) { return OR; }
(?i:ORDER) { return ORDER; }
(?i:OUTER) { return OUTER; }
(?i:PRIMARY) { return PRIMARY; }
(?i:RIGHT) { return RIGHT; }
(?i:SELECT) { return SELECT; }
(?i:SMALLINT) { return SMALLINT; }
(?i:SUM) { return SUM; }
(?i:TABLE) { return TABLE; }
(?i:TRUE) { return TRUE; }
(?i:UNION) { return UNION; }
(?i:UNIQUE) { return UNIQUE; }
(?i:UNKNOWN) { return UNKNOWN; }
(?i:USING) { return USING; }
(?i:VALUES) { return VALUES; }
(?i:VARCHAR) { return VARCHAR; }
(?i:VARYING) { return VARYING; }
(?i:WHERE) { return WHERE; }
(?i:NULL) { return NULL_TOKEN; }

, { return COMMA; }
\( { return LEFT_PAREN; }
\) { return RIGHT_PAREN; }
\; { return SEMICOLON; }
\+ { return PLUS; }
- { return MINUS; }
\* { return ASTERISK; }
\/ { return SOLIDUS; }
\. { return PERIOD; }
\= { return EQUALS; }
\<= { return LESS_THAN_OR_EQUALS; }
\>= { return GREATER_THAN_OR_EQUALS; }
\<\> { return NOT_EQUALS; }
\< { return LESS_THAN; }
\> { return GREATER_THAN; }
\| { return PIPE; }


[0-9][0-9]* { return UNSIGNED_INTEGER; }
[a-zA-Z][a-zA-Z0-9_]* { return IDENTIFIER_START; }

[ \t\r\n]      ;  /* skip whitespace which is not part of a string */

\" { BEGIN(STRING); }
<STRING>[^(\"\")]* { /* This should go into a string*/ }
<STRING>\" { BEGIN(INITIAL); return UNSIGNED_INTEGER; }

` { BEGIN(IDENTIFIER); }
<IDENTIFIER>[^(``)]* { /* This should go into a string*/ }
<IDENTIFIER>` { BEGIN(INITIAL); return IDENTIFIER_START; }

<<EOF>> { return ENDOFFILE; }

%%
