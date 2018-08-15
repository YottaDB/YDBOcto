%require "3.0"
%debug

%{
#include <stdio.h>
#include <stdlib.h>

#define YYERROR_VERBOSE
#define YYSTYPE struct statement *
struct statement;

extern FILE* yyin;
extern int yylex();
extern int yyparse();
extern void yyerror(char const *s);
extern char *yytext;
%}

%expect 0

%token CHAR
%token CHARACTER
%token COMMAND
%token CREATE
%token DEC
%token DECIMAL
%token IDENTIFIER_START
%token INT
%token INTEGER
%token NUMERIC
%token SMALLINT
%token TABLE
%token VARCHAR
%token VARYING

%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON

%token UNSIGNED_INTEGER

%%

sql_schema_statement
  : sql_schema_definition_statement SEMICOLON { YYACCEPT; }
//  | sql_schema_manipulation_statement
  ;

sql_schema_definition_statement
  : table_definition
  ;

/// TODO: not complete
table_definition
  : CREATE TABLE table_name table_element_list
  ;

table_name
  : qualified_name
//  | qualified_local_table_name
  ;

table_element_list
  : LEFT_PAREN table_element table_element_list_tail RIGHT_PAREN
  ;

table_element_list_tail
  : /* Empty */
  | COMMA table_element table_element_list_tail
  ;

table_element
  : column_definition
//  | table_constraint_definition
  ;

/// TODO: not complete
column_definition
  : column_name data_type
//  | more stuff
  ;

column_name
  : identifier
  ;

qualified_name
  : qualified_identifier
//  | schema_name period qualified_identifier
  ;

qualified_identifier
  : identifier
  ;

identifier
  : actual_identifier
//  | introducer character_set_specification actual_identifier
  ;

actual_identifier
  : regular_identifier
//  | delimited_identifier
  ;

regular_identifier
  : identifier_body
  ;

identifier_body
  : IDENTIFIER_START
//  | identifier_start underscore
//  | identifier_start identifier_part
  ;

data_type
  : character_string_type
//  | character_string_type CHARACTER SET character_set_specification
//  | national_character_string_type
//  | bit_string_type
  | numeric_type
//  | datetime_type
//  | interval_type
  ;

character_string_type
  : CHARACTER character_string_type_char_tail
  | CHAR character_string_type_char_tail
  | CHARACTER VARYING character_string_type_char_tail
  | CHAR VARYING character_string_type_char_tail
  | VARCHAR character_string_type_char_tail
  ;

character_string_type_char_tail
  : /* Empty */
  | LEFT_PAREN length RIGHT_PAREN
  ;

length
  : UNSIGNED_INTEGER
  ;

numeric_type
  : exact_numeric_type
//  | approximate_numeric_type
  ;

exact_numeric_type
  : NUMERIC exact_numeric_type_tail
  | DECIMAL exact_numeric_type_tail
  | DEC exact_numeric_type_tail
  | INTEGER
  | INT
  | SMALLINT
  ;

exact_numeric_type_tail
  : /* Empty */
  | LEFT_PAREN precision exact_numeric_type_tail_tail RIGHT_PAREN
  ;

exact_numeric_type_tail_tail
  : /* Empty */
  | COMMAND scale
  ;

precision
  : UNSIGNED_INTEGER
  ;

scale
  : UNSIGNED_INTEGER
  ;

%%
