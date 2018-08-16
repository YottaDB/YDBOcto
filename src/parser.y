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
%require "3.0"
%debug

%code requires {
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif
}

%{
#include <stdio.h>
#include <stdlib.h>

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

#define YYERROR_VERBOSE
#define YYDEBUG 1
#define YYSTYPE struct statement *
struct abc {
  int a;
} statement;

extern FILE* yyin;
extern int yylex();
extern int yyparse(yyscan_t scan);
extern void yyerror(yyscan_t scan, char const *s);
extern char *yytext;
%}

%define api.pure
%lex-param   { yyscan_t scanner }
%parse-param { yyscan_t scanner }

%token ALL
%token AND
%token BY
%token CHAR
%token CHARACTER
%token COMMAND
%token CORRESPONDING
%token CREATE
%token COLLATE
%token DEC
%token DECIMAL
%token DEFAULT
%token DELETE
%token EXCEPT
%token FALSE
%token FROM
%token IDENTIFIER_START
%token IN
%token INT
%token INTEGER
%token INTERSECT
%token IS
%token KEY
%token NOT
%token NUMERIC
%token OR
%token PRIMARY
%token SMALLINT
%token TABLE
%token TRUE
%token UNION
%token UNIQUE
%token UNKNOWN
%token VARCHAR
%token VARYING
%token VALUES
%token WHERE

%token NULL_TOKEN
%token ENDOFFILE
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token PLUS
%token MINUS
%token ASTERISK
%token SOLIDUS
%token PERIOD
%token EQUALS
%token NOT_EQUALS
%token LESS_THAN
%token GREATER_THAN
%token LESS_THAN_OR_EQUALS
%token GREATER_THAN_OR_EQUALS
%token PIPE

%token UNSIGNED_INTEGER

%%

sql_statement
  : sql_schema_statement SEMICOLON { YYACCEPT; }
  | sql_data_statement SEMICOLON { YYACCEPT; }
  | ENDOFFILE { YYACCEPT; }
  ;

sql_data_statement
  : sql_data_change_statement
//  | open_statement
//  | fetch_statement
//  | close_statement
//  | select_statement_single_row
  ;

sql_data_change_statement
  : delete_statement_searched
//  | delete_statement_position
//  | insert_statement
//  | update_statement_positioned
//  | update_statement_searched
  ;

delete_statement_searched
  : DELETE FROM table_name delete_statement_searched_tail
  ;

delete_statement_searched_tail
  : /* Empty */
  | WHERE search_condition
  ;

search_condition
  : boolean_term
  | search_condition OR boolean_term
  ;

boolean_term
  : boolean_factor
  | boolean_term AND boolean_factor
  ;

boolean_factor
  : boolean_test
  | NOT boolean_test
  ;

boolean_test
  : boolean_primary boolean_test_tail
  ;

boolean_test_tail
  : /* Empty */
  | IS boolean_test_tail_tail
  ;

boolean_test_tail_tail
  : truth_value
  | NOT truth_value
  ;

truth_value
  : TRUE
  | FALSE
  | UNKNOWN
  ;

boolean_primary
  : predicate
  | LEFT_PAREN search_condition RIGHT_PAREN
  ;

predicate
  : comparison_predicate
//  | between_predicate
  | in_predicate
//  | like_predicate
//  | null_predicate
//  | quantified_comparison_predicate
//  | exists_predicate
//  | match_predicate
//  | overlaps_predicate
  ;

comparison_predicate
  : row_value_constructor comp_op row_value_constructor
  ;

in_predicate
  : row_value_constructor in_not_in in_predicate_value
  ;

in_not_in
  : IN
  | NOT IN
  ;

in_predicate_value
  : table_subquery
  | LEFT_PAREN in_value_list RIGHT_PAREN
  ;

table_subquery
  : subquery
  ;

in_value_list
  : value_expression in_value_list_tail
  ;

in_value_list_tail
  : /* Empty */
  | COMMA in_value_list
  ;

/* !! deviations from BNF due to rr conflicts
*/
row_value_constructor
  : VALUES LEFT_PAREN row_value_constructor_list RIGHT_PAREN
  | row_value_constructor_element
  ;

row_value_constructor_list
  : row_value_constructor_element row_value_constructor_list_tail
  ;

row_value_constructor_list_tail
  : /* Empty */
  | COMMA row_value_constructor_list
  ;

row_value_constructor_element
  : value_expression
  | null_specification
  | default_specification
  ;

/* The runtime system is responsible for ensuring
    types, as we need knowledge of column types
*/
value_expression
  : numeric_value_expression
//  | datetime_value_expression
//  | interval_expression
  ;

null_specification
  : NULL_TOKEN
  ;

default_specification
  : DEFAULT
  ;

numeric_value_expression
  : term
  | numeric_value_expression PLUS term
  | numeric_value_expression MINUS term
  ;

term
  : factor
  | term ASTERISK factor
  | term SOLIDUS factor
  | term concatenation_operator factor

concatenation_operator
  : PIPE PIPE
  ;

factor
  : sign numeric_primary factor_tail
  | numeric_primary factor_tail
  ;

factor_tail
  : /* Empty */
  | collate_clause
  ;

collate_clause
  : COLLATE collation_name
  ;

collation_name
  : qualified_name
  ;

sign
  : PLUS
  | MINUS
  ;

numeric_primary
  : value_expression_primary
//  | numeric_value_function
  ;

/* There is a reduce/reduce conflict here caused by
 *  not knowing if the columns in question are numeric
 *  or strings; the expansion ends up being pretty similar
 */
value_expression_primary
  : unsigned_value_specification
  | column_reference
//  | set_function_specification
  | scalar_subquery
//  | case_expression
  | LEFT_PAREN value_expression RIGHT_PAREN
//  | cast_specification
  ;

unsigned_value_specification
  : unsigned_literal
//  | general_value_specification
  ;

unsigned_literal
  : unsigned_numeric_literal
//  | general_literal
  ;

unsigned_numeric_literal
  : exact_numeric_literal
//  | approximate_numeric_literal
  ;

exact_numeric_literal
  : UNSIGNED_INTEGER exact_numeric_literal_tail
  | exact_numeric_literal_tail_tail
  ;

exact_numeric_literal_tail
  : /* Empty */
  | exact_numeric_literal_tail_tail
  ;

exact_numeric_literal_tail_tail
  : PERIOD UNSIGNED_INTEGER
  ;

column_reference
  : qualifier PERIOD column_name
  | column_name
  ;

qualifier
  : table_name
//  | correlation_name
  ;

scalar_subquery
  : subquery
  ;

subquery
  : LEFT_PAREN query_expression RIGHT_PAREN
  ;

query_expression
  : non_join_query_expression
//  | joined_table
  ;

non_join_query_expression
  : non_join_query_term
  | query_expression UNION non_join_query_expression_tail
  | query_expression EXCEPT non_join_query_expression_tail
  ;

non_join_query_expression_tail
  : ALL non_join_query_expression_tail_tail query_term
  | query_term
  ;

non_join_query_expression_tail_tail
  : /* Empty */
  | CORRESPONDING non_join_query_expression_tail_tail_tail
  ;

non_join_query_expression_tail_tail_tail
  : /* Empty */
  | BY LEFT_PAREN corresponding_column_list RIGHT_PAREN
  ;

corresponding_column_list
  : column_name_list
  ;

column_name_list
  : column_name column_name_list_tail
  ;

column_name_list_tail
  : /* Empty */
  | COMMA column_name_list
  ;

query_term
  : non_join_query_term
//  | joined_table
  ;

comp_op
  : EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | GREATER_THAN
  | LESS_THAN_OR_EQUALS
  | GREATER_THAN_OR_EQUALS
  ;

non_join_query_term
  : non_join_query_primary
  | query_term INTERSECT query_term_tail query_primary
  ;

query_term_tail
  : ALL corresponding_spec
  | corresponding_spec
  ;

corresponding_spec
  : CORRESPONDING corresponding_spec_tail
  | /* Empty */
  ;

corresponding_spec_tail
  : BY LEFT_PAREN corresponding_column_list RIGHT_PAREN
  ;

non_join_query_primary
  : simple_table
  | LEFT_PAREN non_join_query_expression RIGHT_PAREN
  ;

simple_table
  : table_value_constructor
  | explicit_table
//  | query_specification // this can be enabled after SELECT is implemented
  ;

table_value_constructor
  : VALUES table_value_constructor_list
  ;

table_value_constructor_list
  : row_value_constructor table_value_constructor_list_tail
  ;

table_value_constructor_list_tail
  : /* Empty */
  | COMMA table_value_constructor_list
  ;

explicit_table
  : TABLE table_name
  ;

query_primary
  : non_join_query_primary
//  | joined_table
  ;

sql_schema_statement
  : sql_schema_definition_statement
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
  : column_name data_type column_definition_tail
//  | more stuff
  ;

column_name
  : identifier
  ;

column_definition_tail
  : /* Empty */
  | column_constraint_definition
  ;

column_constraint_definition
  : constraint_name_definition column_constraint constraint_attributes
  ;

/// TODO: not complete
constraint_name_definition
  : /* Empty */
  ;

/// TODO: not complete
column_constraint
  : NOT NULL_TOKEN
  | unique_specifications
//  | reference_specifications
//  | check_constraint_definition
  ;

unique_specifications
  : UNIQUE
  | PRIMARY KEY
  ;

/// TODO: not complete
constraint_attributes
  : /* Empty */
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
