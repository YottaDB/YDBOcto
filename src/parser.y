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
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "parser.h"

#define YYERROR_VERBOSE
#define YYDEBUG 1
#define YYSTYPE SqlStatement *

extern int yylex(YYSTYPE * yylval_param, YYLTYPE *llocp, yyscan_t yyscanner);
extern int yyparse(yyscan_t scan, SqlStatement **out);
extern void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, char const *s);

%}

%define api.pure full
%locations
%lex-param   { yyscan_t scanner }
%parse-param { yyscan_t scanner } { SqlStatement **out }

%token ALL
%token AND
%token AS
%token ASC
%token AVG
%token BY
%token CASCADE
%token CHAR
%token CHARACTER
%token COLLATE
%token COMMAND
%token CORRESPONDING
%token COUNT
%token CREATE
%token CROSS
%token CURSOR
%token DEC
%token DECIMAL
%token DEFAULT
%token DELETE
%token DELIM
%token DESC
%token DISTINCT
%token DROP
%token END
%token EXCEPT
%token EXTRACT
%token FALSE_TOKEN
%token FROM
%token FULL
%token GLOBAL
%token GROUP
%token HAVING
%token IDENTIFIER_START
%token IN
%token INNER
%token INSERT
%token INT
%token INTEGER
%token INTERSECT
%token INTO
%token IS
%token JOIN
%token KEY
%token LEFT
%token MAX
%token MIN
%token NATURAL
%token NOT
%token NUMERIC
%token ON
%token OR
%token ORDER
%token OUTER
%token PACK
%token PIECE
%token PRIMARY
%token RESTRICT
%token RIGHT
%token SELECT
%token SET
%token SMALLINT
%token SUM
%token TABLE
%token TRUE_TOKEN
%token UNION
%token UNIQUE
%token UNKNOWN
%token UNPACK
%token UPDATE
%token USING
%token VALUES
%token VARCHAR
%token VARYING
%token WHERE
%token NUM
%token ADVANCE

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

%token LITERAL
%token FAKE_TOKEN
%token INVALID_TOKEN

%%

sql_statement
  : sql_schema_statement SEMICOLON { *out = $1; YYACCEPT; }
  | sql_data_statement SEMICOLON { *out = $1; YYACCEPT; }
  | sql_select_statement SEMICOLON { *out = $1; YYACCEPT; }
  | ENDOFFILE { YYACCEPT; }
  ;

%include "parser/select.y"
%include "parser/insert.y"
%include "parser/update.y"
%include "parser/drop.y"

sql_data_statement
  : sql_data_change_statement { $$ = $1; }
//  | open_statement
//  | fetch_statement
//  | close_statement
//  | select_statement_single_row
  ;

sql_data_change_statement
  : delete_statement_searched
//  | delete_statement_position
  | insert_statement { $$ = $1; }
//  | update_statement_positioned
  | update_statement_searched
  ;

delete_statement_searched
  : DELETE FROM column_name delete_statement_searched_tail
  ;

delete_statement_searched_tail
  : /* Empty */
  | WHERE search_condition
  ;

search_condition
  : boolean_term {$$ = $boolean_term; }
  | search_condition OR boolean_term  {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_OR;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

boolean_term
  : boolean_factor { $$ = $boolean_factor; }
  | boolean_term AND boolean_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_AND;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

boolean_factor
  : boolean_test { $$ = $1; }
  | NOT boolean_test {
      $$ = (SqlStatement*)malloc(sizeof(SqlStatement));
      ($$)->type = unary_STATEMENT;
      ($$)->v.unary = (SqlUnaryOperation*)malloc(sizeof(SqlUnaryOperation));
      ($$)->v.unary->operation = BOOLEAN_NOT;
      ($$)->v.unary->operand = ($2);
    }
  ;

boolean_test
  : boolean_primary boolean_test_tail {
      if($boolean_test_tail != NULL) {
        SQL_STATEMENT($$, binary_STATEMENT);
        ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
        ($$)->v.binary->operation = BOOLEAN_IS;
        ($$)->v.binary->operands[0] = ($1);
        ($$)->v.binary->operands[1] = ($2);
      } else {
        $$ = $boolean_primary;
      }
    }
  ;

boolean_test_tail
  : /* Empty */ { $$ = NULL; }
  | IS boolean_test_tail_tail
  ;

boolean_test_tail_tail
  : truth_value
  | NOT truth_value
  ;

truth_value
  : TRUE_TOKEN
  | FALSE_TOKEN
  | UNKNOWN
  ;

boolean_primary
  : predicate { $$ = $predicate; }
  | LEFT_PAREN search_condition RIGHT_PAREN { $$ = $search_condition; }
  ;

predicate
  : comparison_predicate { $$ = $1; }
//  | between_predicate
  | in_predicate { $$ = $1; }
//  | like_predicate
//  | null_predicate
//  | quantified_comparison_predicate
//  | exists_predicate
//  | match_predicate
//  | overlaps_predicate
  ;

comparison_predicate
  : row_value_constructor EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor NOT_EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_NOT_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor LESS_THAN row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_LESS_THAN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor GREATER_THAN row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_GREATER_THAN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor LESS_THAN_OR_EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_LESS_THAN_OR_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor GREATER_THAN_OR_EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_GREATER_THAN_OR_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

in_predicate
  : row_value_constructor IN in_predicate_value {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_IN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor NOT IN in_predicate_value {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = BOOLEAN_NOT_IN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

/// TODO: these require additional structures in octo_types.h
in_predicate_value
  : table_subquery
  | LEFT_PAREN in_value_list RIGHT_PAREN
  ;

table_subquery
  : subquery { $$ = $1; }
  ;

in_value_list
  : value_expression in_value_list_tail
  ;

in_value_list_tail
  : /* Empty */
  | COMMA in_value_list
  ;

row_value_constructor
  : LEFT_PAREN row_value_constructor_list RIGHT_PAREN
  | row_value_constructor_element
  ;

row_value_constructor_subquery
  : query_expression { $$ = $1; }
  ;

row_value_constructor_list
  : row_value_constructor_element row_value_constructor_list_tail
  | row_value_constructor_subquery
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
  : numeric_value_expression { $$ = $1; }
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
  : term { $$ = $1; }
  | numeric_value_expression PLUS term {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = ADDITION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | numeric_value_expression MINUS term {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = SUBTRACTION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

term
  : factor { $$ = $1; }
  | term ASTERISK factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = MULTIPLICATION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | term SOLIDUS factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = DVISION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | term concatenation_operator factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = CONCAT;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

concatenation_operator
  : PIPE PIPE
  ;

/// TODO: collate_clause is thoroughly ignored below
factor
  : PLUS numeric_primary factor_tail {
      $$ = (SqlStatement*)malloc(sizeof(SqlStatement));
      ($$)->type = unary_STATEMENT;
      ($$)->v.unary = (SqlUnaryOperation*)malloc(sizeof(SqlUnaryOperation));
      ($$)->v.unary->operation = FORCE_NUM;
      ($$)->v.unary->operand = ($2);
    }
  | MINUS numeric_primary factor_tail {
      $$ = (SqlStatement*)malloc(sizeof(SqlStatement));
      ($$)->type = unary_STATEMENT;
      ($$)->v.unary = (SqlUnaryOperation*)malloc(sizeof(SqlUnaryOperation));
      ($$)->v.unary->operation = NEGATIVE;
      ($$)->v.unary->operand = ($2);
    }
  | numeric_primary factor_tail { $$ = $1; }
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

numeric_primary
  : value_expression_primary { $$ = $1; }
//  | numeric_value_function
  ;

/* There is a reduce/reduce conflict here caused by
 *  not knowing if the columns in question are numeric
 *  or strings; the expansion ends up being pretty similar
 */
value_expression_primary
  : literal_value { $$ = $1; }
  | column_reference
  | set_function_specification
  | scalar_subquery
//  | case_expression
  | LEFT_PAREN value_expression RIGHT_PAREN
//  | cast_specification
  ;

set_function_specification
  : COUNT LEFT_PAREN ASTERISK RIGHT_PAREN
  | COUNT LEFT_PAREN value_expression RIGHT_PAREN
  | COUNT LEFT_PAREN set_quantifier value_expression RIGHT_PAREN
  | general_set_function
  ;

general_set_function
  : set_function_type LEFT_PAREN value_expression RIGHT_PAREN
  | set_function_type LEFT_PAREN set_quantifier value_expression RIGHT_PAREN
  ;

set_function_type
  : AVG
  | MAX
  | MIN
  | SUM
  ;

non_query_value_expression
  : non_query_numeric_value_expression {
      SQL_STATEMENT($$, value_STATEMENT);
      ($$)->v.value = (SqlValue*)malloc(sizeof(SqlValue));
      ($$)->v.value->type = CALCULATED_VALUE;
      ($$)->v.value->v.calculated = $1;
    }
  ;

non_query_numeric_value_expression
  : non_query_term { $$ = $1; }
  | non_query_numeric_value_expression PLUS non_query_term {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = ADDITION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | non_query_numeric_value_expression MINUS non_query_term {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = SUBTRACTION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

non_query_term
  : non_query_factor { $$ = $1; }
  | non_query_term ASTERISK non_query_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = MULTIPLICATION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | non_query_term SOLIDUS non_query_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = DVISION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | non_query_term concatenation_operator non_query_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      ($$)->v.binary = (SqlBinaryOperation*)malloc(sizeof(SqlBinaryOperation));
      ($$)->v.binary->operation = CONCAT;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

non_query_factor
  : PLUS non_query_numeric_primary factor_tail {
      SQL_STATEMENT($$, unary_STATEMENT);
      ($$)->v.unary = (SqlUnaryOperation*)malloc(sizeof(SqlUnaryOperation));
      ($$)->v.unary->operation = FORCE_NUM;
      ($$)->v.unary->operand = ($2);
    }
  | MINUS non_query_numeric_primary factor_tail {
      SQL_STATEMENT($$, unary_STATEMENT);
      ($$)->v.unary = (SqlUnaryOperation*)malloc(sizeof(SqlUnaryOperation));
      ($$)->v.unary->operation = NEGATIVE;
      ($$)->v.unary->operand = ($2);
    }
  | non_query_numeric_primary factor_tail { $$ = $1; }
  ;

non_query_numeric_primary
  : non_query_value_expression_primary { $$ = $1; }
//  | numeric_value_function
  ;

non_query_value_expression_primary
  : literal_value { $$ = $1; }
  | column_reference
  | set_function_specification
  | LEFT_PAREN non_query_value_expression RIGHT_PAREN
  ;

column_reference
  : qualifier PERIOD column_name
  | column_name
  ;

qualifier
  : column_name
  ;

scalar_subquery
  : subquery
  ;

subquery
  : LEFT_PAREN query_expression RIGHT_PAREN { $$ = $2; }
  ;

query_expression
  : non_join_query_expression { $$ = $1; }
  | joined_table { $$ = $1; }
  ;

non_join_query_expression
  : non_join_query_term { $$ = $1; }
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
  | joined_table
  ;

non_join_query_term
  : non_join_query_primary {$$ = $1; }
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
  : simple_table {$$ = $1; }
  | LEFT_PAREN non_join_query_expression RIGHT_PAREN
  ;

simple_table
  : table_value_constructor
  | explicit_table
  | query_specification { $$ = $1; }
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
  : TABLE column_name
  ;

query_primary
  : non_join_query_primary
  | joined_table
  ;

sql_schema_statement
  : sql_schema_definition_statement { $$ = $1; }
  | sql_schema_manipulation_statement
  ;

/// TODO: not complete
sql_schema_manipulation_statement
  : drop_table_statement
  ;

sql_schema_definition_statement
  : table_definition { $$ = $1; }
  ;

/// TODO: not complete
table_definition
  : CREATE TABLE column_name LEFT_PAREN table_element_list RIGHT_PAREN table_definition_tail {
        SQL_STATEMENT($$, table_STATEMENT);
        ($$)->v.table = (SqlTable*)malloc(sizeof(SqlTable));
        memset(($$)->v.table, 0, sizeof(SqlTable));
        assert($column_name->type == value_STATEMENT
          && $column_name->v.value->type == COLUMN_REFERENCE);
        ($$)->v.table->tableName = $column_name;
        ($$)->v.table->columns = $table_element_list;
        if(create_table_defaults($$, $table_definition_tail)) {
          YYABORT;
        }
        dqinit(($$)->v.table);
      }
  ;

table_definition_tail
  : /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | optional_keyword { $$ = $1; }
  ;

optional_keyword
  : optional_keyword_element optional_keyword_tail {
      $$ = $optional_keyword_element;
      SqlOptionalKeyword *keyword, *t_keyword;
      UNPACK_SQL_STATEMENT(keyword, $optional_keyword_tail, keyword);
      dqinsert(keyword, ($$)->v.keyword, t_keyword);
      free($optional_keyword_tail);
    }
  ;

optional_keyword_element
  : GLOBAL literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_SOURCE;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
    }
  | CURSOR literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_CURSE;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
    }
  | END literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_END;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
    }
  | UNPACK literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_START;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
    }
  | DELIM literal_value {
       SQL_STATEMENT($$, keyword_STATEMENT);
       ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
       ($$)->v.keyword->keyword = OPTIONAL_DELIM;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);
     }
 | PACK literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_PACK;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
    }
  ;

optional_keyword_tail
  : /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | optional_keyword { assert($optional_keyword->type == keyword_STATEMENT); $$ = $optional_keyword; }
  ;

table_element_list
  :  table_element table_element_list_tail  {
      $$ = $table_element;
      assert($$->type = column_STATEMENT);
      if($table_element_list_tail)
      {
        SqlColumn *t_column;
        assert($table_element_list_tail->type = column_STATEMENT);
        dqinsert($table_element_list_tail->v.column, ($$)->v.column, t_column);
        free($table_element_list_tail);
      }
    }
  ;

table_element_list_tail
  : /* Empty */ { $$ = 0; }
  | COMMA table_element_list { $$ = $table_element_list; }
  ;

table_element
  : column_definition { $$ = $1; }
//  | table_constraint_definition
  ;

/// TODO: not complete
column_definition
  : column_name data_type column_definition_tail {
      SQL_STATEMENT($$, column_STATEMENT);
      ($$)->v.column = (SqlColumn*)malloc(sizeof(SqlColumn));
      dqinit(($$)->v.column);
      ($$)->v.column->columnName = $column_name;
      assert($data_type->type == data_type_STATEMENT);
      ($$)->v.column->type = $data_type->v.data_type;
      cleanup_sql_statement($data_type);
      ($$)->v.column->tableName = NULL;
      ($$)->v.column->keywords = $column_definition_tail;
    }
//  | more stuff
  ;

column_name
  : identifier  { $$ = $1; }
  ;

column_definition_tail
  : /* Empty */ {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       dqinit(($$)->v.keyword);
    }
  | EXTRACT literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_EXTRACT;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $3, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
       free($3);
    }
  | PIECE literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_PIECE;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $3, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
       free($3);
    }
  | DELIM literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_DELIM;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $3, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
       free($3);
     }
  | GLOBAL literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_SOURCE;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $3, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
       free($3);
    }
  | KEY NUM literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_KEY_NUM;
       ($$)->v.keyword->v = $3;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $4, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
       free($4);
    }
  | ADVANCE literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_ADVANCE;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $3, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
       free($3);
    }
  | column_constraint_definition column_definition_tail {
       $$ = $column_constraint_definition;
       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $2, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
       free($2);
    }
  ;

column_constraint_definition
  : constraint_name_definition column_constraint constraint_attributes {
      ($$) = $column_constraint;
    }
  ;

/// TODO: not complete
constraint_name_definition
  : /* Empty */
  ;

/// TODO: not complete
column_constraint
  : NOT NULL_TOKEN {
      SQL_STATEMENT($$, keyword_STATEMENT);
      MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = NOT_NULL;
      dqinit(($$)->v.keyword);
    }
  | unique_specifications { $$ = $1; }
//  | reference_specifications
//  | check_constraint_definition
  ;

unique_specifications
  : UNIQUE {
      SQL_STATEMENT($$, keyword_STATEMENT);
      MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = UNIQUE_CONSTRAINT;
      dqinit(($$)->v.keyword);
    }
  | PRIMARY KEY {
      SQL_STATEMENT($$, keyword_STATEMENT);
      MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = PRIMARY_KEY;
      dqinit(($$)->v.keyword);
    }
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
  : actual_identifier { $$ = $1; }
//  | introducer character_set_specification actual_identifier
  ;

actual_identifier
  : regular_identifier { $$ = $1; }
//  | delimited_identifier
  ;

regular_identifier
  : identifier_body { $$ = $1; }
  ;

identifier_body
  : IDENTIFIER_START { $$ = $1; ($$)->loc =  yyloc; }
//  | identifier_start underscore
//  | identifier_start identifier_part
  ;

data_type
  : character_string_type {
      SQL_STATEMENT($$, data_type_STATEMENT);
      ($$)->v.data_type = CHARACTER_STRING_TYPE;
      cleanup_sql_statement($1); // This should be added as a constraint
    }
//  | character_string_type CHARACTER SET character_set_specification
//  | national_character_string_type
//  | bit_string_type
  | numeric_type {
      SQL_STATEMENT($$, data_type_STATEMENT);
      ($$)->v.data_type = INTEGER_TYPE;
      if($numeric_type) {
        cleanup_sql_statement($numeric_type);
      }
    }
//  | datetime_type
//  | interval_type
  ;

// These should be implemented as constraints
character_string_type
  : CHARACTER character_string_type_char_tail { $$ = $2; }
  | CHAR character_string_type_char_tail { $$ = $2; }
  | CHARACTER VARYING character_string_type_char_tail { $$ = $2; }
  | CHAR VARYING character_string_type_char_tail { $$ = $2; }
  | VARCHAR character_string_type_char_tail { $$ = $2; }
  ;

character_string_type_char_tail
  : /* Empty */ { $$ = NULL; }
  | LEFT_PAREN length RIGHT_PAREN { $$ = $2; }
  ;

length
  : literal_value { $$ = $1; }
  ;

numeric_type
  : exact_numeric_type { $$ = $1; }
//  | approximate_numeric_type
  ;

exact_numeric_type
  : NUMERIC exact_numeric_type_tail { $$ = $2; }
  | DECIMAL exact_numeric_type_tail { $$ = $2; }
  | DEC exact_numeric_type_tail { $$ = $2; }
  | INTEGER { $$ = NULL; }
  | INT { $$ = NULL; }
  | SMALLINT { $$ = NULL; }
  ;


/// TODO: we should have a triple for this type of numeric which includes scale
exact_numeric_type_tail
  : /* Empty */ { $$ = NULL; }
  | LEFT_PAREN precision exact_numeric_type_tail_tail RIGHT_PAREN {
      $$ = $precision;
      if($exact_numeric_type_tail_tail)
        cleanup_sql_statement($exact_numeric_type_tail_tail);
    }
  ;

exact_numeric_type_tail_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA scale { $$ = $2; }
  ;

precision
  : literal_value { $$ = $1; }
  ;

scale
  : literal_value { $$ = $1; }
  ;

literal_value
  : LITERAL { $$ = $1; ($$)->loc = yyloc; }

%%
