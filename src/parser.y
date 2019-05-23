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

// Included for function lp_columns_to_column_list
//  this function should be moved
#include "logical_plan.h"

#define YYERROR_VERBOSE
#define YYDEBUG 1
#define YYSTYPE SqlStatement *

extern int yylex(YYSTYPE * yylval_param, YYLTYPE *llocp, yyscan_t yyscanner);
extern int yyparse(yyscan_t scan, SqlStatement **out, int *plan_id);
extern void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, char const *s);

%}

%define api.pure full
%locations
%lex-param   { yyscan_t scanner }
%parse-param { yyscan_t scanner } { SqlStatement **out } { int *plan_id }

%token ADVANCE
%token ALL
%token AND
%token AS
%token ASC
%token AVG
%token BEG
%token BY
%token CASCADE
%token CASE
%token CHAR
%token CHARACTER
%token COLLATE
%token COMMAND
%token COMMIT
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
%token ELSE
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
%token LIMIT
%token MAX
%token MIN
%token NATURAL
%token NOT
%token NUM
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
%token SHOW
%token SMALLINT
%token START
%token SUM
%token TABLE
%token THEN
%token TO
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
%token WHEN
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
%token TILDE
%token EXCLAMATION

%token LITERAL
%token FAKE_TOKEN
%token INVALID_TOKEN

%%

sql_statement
  : sql_schema_statement semicolon_or_eof { *out = $1; YYACCEPT; }
  | sql_data_statement semicolon_or_eof { *out = $1; YYACCEPT; }
  | query_expression semicolon_or_eof { *out = $1; YYACCEPT; }
  | BEG semicolon_or_eof {
      // For now, we don't do transaction, so just say OK to this word
      SQL_STATEMENT(*out, begin_STATEMENT);
      YYACCEPT;
    }
  | COMMIT semicolon_or_eof {
      SQL_STATEMENT(*out, commit_STATEMENT);
      YYACCEPT;
    }
  | error semicolon_or_eof { *out = NULL; YYABORT; }
  | sql_set_statement semicolon_or_eof { *out = $1; YYACCEPT; }
  | semicolon_or_eof {
      SQL_STATEMENT(*out, no_data_STATEMENT);
      eof_hit = TRUE;
      YYACCEPT;
    }
  ;

semicolon_or_eof
  : SEMICOLON
  | ENDOFFILE { eof_hit = TRUE; }
  ;

%include "parser/select.y"
%include "parser/insert.y"
%include "parser/update.y"
%include "parser/drop.y"
%include "parser/set.y"

sql_data_statement
  : sql_data_change_statement { $$ = $1; }
//  | open_statement
//  | fetch_statement
//  | close_statement
//  | select_statement_single_row
  ;

sql_data_change_statement

  : delete_statement_searched { $$ = $1; }
//  | delete_statement_position
  | insert_statement { $$ = $1; }
//  | update_statement_positioned
  | update_statement_searched { $$ = $1; }
  ;

delete_statement_searched
  : DELETE FROM column_name delete_statement_searched_tail { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "DELETE FROM"); YYABORT; }
  ;

delete_statement_searched_tail
  : /* Empty */ { $$ = NULL; }
  | WHERE search_condition { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "WHERE search_condition"); YYABORT; }
  ;

search_condition
  : boolean_term {$$ = $boolean_term; }
  | search_condition OR boolean_term  {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_OR;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

boolean_term
  : boolean_factor { $$ = $boolean_factor; }
  | boolean_term AND boolean_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_AND;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

boolean_factor
  : boolean_test { $$ = $1; }
  | NOT boolean_test {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.unary->operation = BOOLEAN_NOT;
      ($$)->v.unary->operand = ($2);
    }
  ;

boolean_test
  : boolean_primary boolean_test_tail {
      if($boolean_test_tail != NULL) {
        SQL_STATEMENT($$, binary_STATEMENT);
        MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
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
  | IS boolean_test_tail_tail { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "boolean_test_tail: IS boolean_test_tail_tail"); YYABORT; }
  ;

boolean_test_tail_tail
  : truth_value { $$ = $1; }
  | NOT truth_value { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "boolean_test_tail_tail: NOT truth_value"); YYABORT; }
  ;

truth_value
  : TRUE_TOKEN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "truth_value: TRUE_TOKEN"); YYABORT; }
  | FALSE_TOKEN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "truth_value: FALSE_TOKEN"); YYABORT; }
  | UNKNOWN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "truth_value: UNKNOWN"); YYABORT; }
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
  | null_predicate { $$ = $1; }
//  | quantified_comparison_predicate
//  | exists_predicate
//  | match_predicate
//  | overlaps_predicate
  ;

comparison_predicate
  : row_value_constructor EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor NOT_EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_NOT_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor LESS_THAN row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_LESS_THAN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor GREATER_THAN row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_GREATER_THAN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor LESS_THAN_OR_EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_LESS_THAN_OR_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor GREATER_THAN_OR_EQUALS row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_GREATER_THAN_OR_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor TILDE row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_REGEX_SENSITIVE;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor TILDE ASTERISK row_value_constructor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($4);
    }
  | row_value_constructor EXCLAMATION TILDE row_value_constructor {
      SQL_STATEMENT($$, unary_STATEMENT);
      MALLOC_STATEMENT($$, unary, SqlUnaryOperation);
      SQL_STATEMENT(($$)->v.unary->operand, binary_STATEMENT);
      MALLOC_STATEMENT(($$)->v.unary->operand, binary, SqlBinaryOperation);
      ($$)->v.unary->operation = BOOLEAN_NOT;
      (($$)->v.unary->operand)->v.binary->operation = BOOLEAN_REGEX_SENSITIVE;
      (($$)->v.unary->operand)->v.binary->operands[0] = ($1);
      (($$)->v.unary->operand)->v.binary->operands[1] = ($4);
    }
  | row_value_constructor EXCLAMATION TILDE ASTERISK row_value_constructor {
      SQL_STATEMENT($$, unary_STATEMENT);
      MALLOC_STATEMENT($$, unary, SqlUnaryOperation);
      SQL_STATEMENT(($$)->v.unary->operand, binary_STATEMENT);
      MALLOC_STATEMENT(($$)->v.unary->operand, binary, SqlBinaryOperation);
      ($$)->v.unary->operation = BOOLEAN_NOT;
      (($$)->v.unary->operand)->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE;
      (($$)->v.unary->operand)->v.binary->operands[0] = ($1);
      (($$)->v.unary->operand)->v.binary->operands[1] = ($5);
    }
  ;

in_predicate
  : row_value_constructor IN in_predicate_value {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_IN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | row_value_constructor NOT IN in_predicate_value {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_NOT_IN;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($4);
    }
  ;

/// TODO: these require additional structures in octo_types.h
in_predicate_value
  : table_subquery { $$ = $1; }
  | LEFT_PAREN in_value_list RIGHT_PAREN { $$ = $in_value_list; }
  ;

table_subquery
  : subquery { $$ = $1; }
  ;

in_value_list
  : /* Empty */ {
      SQL_STATEMENT($$, column_list_STATEMENT);
      MALLOC_STATEMENT($$, column_list, SqlColumnList);
      SqlColumnList *column_list;
      UNPACK_SQL_STATEMENT(column_list, $$, column_list);
      dqinit(column_list);
    }

  | value_expression in_value_list_tail {
      SQL_STATEMENT($$, column_list_STATEMENT);
      MALLOC_STATEMENT($$, column_list, SqlColumnList);
      SqlColumnList *column_list, *cl_tail, *cl_temp;
      UNPACK_SQL_STATEMENT(column_list, $$, column_list);
      column_list->value = $value_expression;
      dqinit(column_list);
      if($in_value_list_tail != NULL) {
        UNPACK_SQL_STATEMENT(cl_tail, $in_value_list_tail, column_list);
        dqinsert(column_list, cl_tail, cl_temp);
      }
    }
  ;

in_value_list_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA in_value_list { $$ = $in_value_list; }
  ;

null_predicate
  : row_value_constructor IS NULL_TOKEN {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      SQL_STATEMENT(($$)->v.binary->operands[1], value_STATEMENT);
      MALLOC_STATEMENT(($$)->v.binary->operands[1], value, SqlValue);
      ($$)->v.binary->operands[1]->v.value->type = NUL_VALUE;
      ($$)->v.binary->operands[1]->v.value->v.string_literal = "";
    }
  | row_value_constructor IS NOT NULL_TOKEN {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = BOOLEAN_NOT_EQUALS;
      ($$)->v.binary->operands[0] = ($1);
      SQL_STATEMENT(($$)->v.binary->operands[1], value_STATEMENT);
      MALLOC_STATEMENT(($$)->v.binary->operands[1], value, SqlValue);
      ($$)->v.binary->operands[1]->v.value->type = NUL_VALUE;
      ($$)->v.binary->operands[1]->v.value->v.string_literal = "";
    }

  ;

row_value_constructor
  : LEFT_PAREN row_value_constructor_list RIGHT_PAREN { $$ = $2; }
  | row_value_constructor_element { $$ = $1; }
  ;

row_value_constructor_subquery
  : query_expression { $$ = $1; }
  ;

row_value_constructor_list
  : row_value_constructor_element row_value_constructor_list_tail { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "row_value_constructor_list: element/list_tail"); YYABORT; }
  | row_value_constructor_subquery { $$ = $1; }
  ;

row_value_constructor_list_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA row_value_constructor_list { $$ = $2; }
  ;

row_value_constructor_element
  : value_expression { $$ = $1; }
  | null_specification { $$ = $1; }
  | default_specification { $$ = $1; }
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
  : NULL_TOKEN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "null_specification: NULL_TOKEN"); YYABORT; }
  ;

default_specification
  : DEFAULT { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "default_specification: DEFAULT"); YYABORT; }
  ;

numeric_value_expression
  : term { $$ = $1; }
  | numeric_value_expression PLUS term {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = ADDITION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | numeric_value_expression MINUS term {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = SUBTRACTION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

term
  : factor { $$ = $1; }
  | term ASTERISK factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = MULTIPLICATION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | term SOLIDUS factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = DVISION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | term concatenation_operator factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = CONCAT;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

concatenation_operator
  : PIPE PIPE { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "concatenation_operator: PIPE PIPE"); YYABORT; }
  ;

/// TODO: collate_clause is thoroughly ignored below
factor
  : PLUS numeric_primary factor_tail {
      SQL_STATEMENT($$, unary_STATEMENT);
      MALLOC_STATEMENT($$, unary, SqlBinaryOperation);
      ($$)->v.unary->operation = FORCE_NUM;
      ($$)->v.unary->operand = ($2);
    }
  | MINUS numeric_primary factor_tail {
      SQL_STATEMENT($$, unary_STATEMENT);
      MALLOC_STATEMENT($$, unary, SqlBinaryOperation);
      ($$)->v.unary->operation = NEGATIVE;
      ($$)->v.unary->operand = ($2);
    }
  | numeric_primary factor_tail { $$ = $1; }
  ;

factor_tail
  : /* Empty */ { $$ = NULL; }
  | collate_clause { $$ = $1; }
  ;

collate_clause
  : COLLATE collation_name { $$ = $2; }
  ;

collation_name
  : qualified_name { $$ = $1; }
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
  | column_reference { $$ = $1; }
  | set_function_specification { $$ = $1; }
  | scalar_subquery { $$ = $1; }
//  | case_expression
  | LEFT_PAREN value_expression RIGHT_PAREN { $$ = $2; }
//  | cast_specification
  ;

case_expression
  : case_specification { $$ = $1; }
//  | case_abbreviation
  ;

case_specification
  : simple_case { $$ = $1; }
//  | searched_case
  ;

simple_case
  : CASE value_expression simple_when_clause optional_else_clause END {
      SQL_STATEMENT($$, cas_STATEMENT);
      MALLOC_STATEMENT($$, cas, SqlCaseStatement);
      SqlCaseStatement *cas;
      UNPACK_SQL_STATEMENT(cas, $$, cas);
      cas->value = $value_expression;
      cas->branches = $simple_when_clause;
      cas->optional_else = $optional_else_clause;
    }
  ;

simple_when_clause
  : WHEN value_expression THEN result simple_when_clause_tail {
      SQL_STATEMENT($$, cas_branch_STATEMENT);
      MALLOC_STATEMENT($$, cas_branch, SqlCaseBranchStatement);
      SqlCaseBranchStatement *cas_branch, *tail_cas_branch, *t_cas_branch;
      UNPACK_SQL_STATEMENT(cas_branch, $$, cas_branch);
      cas_branch->condition = $value_expression;
      cas_branch->value = $result;
      dqinit(cas_branch);
      if($simple_when_clause_tail != NULL) {
        UNPACK_SQL_STATEMENT(tail_cas_branch, $simple_when_clause_tail, cas_branch);
        dqinsert(cas_branch, tail_cas_branch, t_cas_branch);
      }
    }
  ;

simple_when_clause_tail
  : /* None */ { $$ = NULL; }
  | simple_when_clause { $$ = $1; }
  ;

optional_else_clause
  : /* Empty */ { $$ = NULL; }
  | ELSE result { $$ = $result; }
  ;

result
  : value_expression { $$ = $1; }
  | NULL_TOKEN {
      SQL_STATEMENT(($$), value_STATEMENT);
      MALLOC_STATEMENT(($$), value, SqlValue);
      ($$)->v.value->type = NUL_VALUE;
      ($$)->v.value->v.string_literal = "";
    }
  ;


set_function_specification
  : COUNT LEFT_PAREN ASTERISK RIGHT_PAREN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "set_function_specification: COUNT LEFT_PAREN ASTERISK RIGHT_PAREN"); YYABORT; }
  | COUNT LEFT_PAREN value_expression RIGHT_PAREN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "set_function_specification: COUNT LEFT_PAREN value_expression RIGHT_PAREN"); YYABORT; }
  | COUNT LEFT_PAREN set_quantifier value_expression RIGHT_PAREN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "set_function_specification: COUNT LEFT_PAREN set_quantifier value_expression RIGHT_PAREN"); YYABORT; }
  | general_set_function { $$ = $1; }
  | generic_function_call { $$ = $1; }
  ;

general_set_function
  : set_function_type LEFT_PAREN value_expression RIGHT_PAREN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "general_set_function: set_function_type LEFT_PAREN value_expression RIGHT_PAREN"); YYABORT; }
  | set_function_type LEFT_PAREN set_quantifier value_expression RIGHT_PAREN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "general_set_function: set_function_type LEFT_PAREN set_quantifier value_expression RIGHT_PAREN"); YYABORT; }
  ;

set_function_type
  : AVG { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "set_function_type: AVG"); YYABORT; }
  | MAX { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "set_function_type: MAX"); YYABORT; }
  | MIN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "set_function_type: MIN"); YYABORT; }
  | SUM { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "set_function_type: SUM"); YYABORT; }
  ;

generic_function_call
  : column_name LEFT_PAREN in_value_list RIGHT_PAREN {
      SQL_STATEMENT($$, value_STATEMENT);
      MALLOC_STATEMENT($$, value, SqlValue);
      SqlStatement *fc_statement;
      SqlFunctionCall *fc;
      SqlValue *value;
      UNPACK_SQL_STATEMENT(value, $$, value);

      value->type = CALCULATED_VALUE;
      SQL_STATEMENT(fc_statement, function_call_STATEMENT);
      MALLOC_STATEMENT(fc_statement, function_call, SqlFunctionCall);
      UNPACK_SQL_STATEMENT(fc, fc_statement, function_call);
      fc->function_name = $column_name;
      fc->parameters = $in_value_list;
      value->v.calculated = fc_statement;

      // Change the value to be a string literal rather than column reference
      UNPACK_SQL_STATEMENT(value, $column_name, value);
      value->type = FUNCTION_NAME;
    }
  ;

non_query_value_expression
  : non_query_numeric_value_expression {
      SQL_STATEMENT($$, value_STATEMENT);
      MALLOC_STATEMENT($$, value, SqlValue);
      ($$)->v.value->type = CALCULATED_VALUE;
      ($$)->v.value->v.calculated = $1;
    }
  ;

non_query_numeric_value_expression
  : non_query_term { $$ = $1; }
  | non_query_numeric_value_expression PLUS non_query_term {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = ADDITION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | non_query_numeric_value_expression MINUS non_query_term {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = SUBTRACTION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

non_query_term
  : non_query_factor { $$ = $1; }
  | non_query_term ASTERISK non_query_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = MULTIPLICATION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | non_query_term SOLIDUS non_query_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = DVISION;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  | non_query_term concatenation_operator non_query_factor {
      SQL_STATEMENT($$, binary_STATEMENT);
      MALLOC_STATEMENT($$, binary, SqlBinaryOperation);
      ($$)->v.binary->operation = CONCAT;
      ($$)->v.binary->operands[0] = ($1);
      ($$)->v.binary->operands[1] = ($3);
    }
  ;

non_query_factor
  : PLUS non_query_numeric_primary factor_tail {
      SQL_STATEMENT($$, unary_STATEMENT);
      MALLOC_STATEMENT($$, unary, SqlUnaryOperation);
      ($$)->v.unary->operation = FORCE_NUM;
      ($$)->v.unary->operand = ($2);
    }
  | MINUS non_query_numeric_primary factor_tail {
      SQL_STATEMENT($$, unary_STATEMENT);
      MALLOC_STATEMENT($$, unary, SqlUnaryOperation);
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
  | column_reference { $$ = $1; }
  | set_function_specification { $$ = $1; }
  | case_expression { $$ = $1; }
  | LEFT_PAREN non_query_value_expression RIGHT_PAREN { $$ = $2; }
  ;

column_reference
  : qualifier PERIOD column_name {
      SqlValue *qual, *col_name;
      char *new_string, *c;
      int len_qual, len_col_name;
      UNPACK_SQL_STATEMENT(qual, $qualifier, value);
      UNPACK_SQL_STATEMENT(col_name, $column_name, value);
      len_qual = strlen(qual->v.string_literal);
      len_col_name = strlen(qual->v.string_literal);
      // +1 for null, +1 for '.'
      new_string = octo_cmalloc(memory_chunks, len_qual + len_col_name + 2);
      c = new_string;
      memcpy(c, qual->v.string_literal, len_qual);
      c += len_qual;
      *c++ = '.';
      memcpy(c, col_name->v.string_literal, len_col_name);
      c += len_col_name;
      *c++ = '\0';
      qual->v.string_literal = new_string;
    }
  | column_name { $$ = $1; }
  ;

qualifier
  : column_name { $$ = $1; }
  ;

scalar_subquery
  : subquery { $$ = $1; }
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
  | query_expression UNION query_term non_join_query_expression_tail_tail {
        $$ = $1;
        assert(($1)->type == select_STATEMENT);
        assert(($3)->type == select_STATEMENT);
        SqlSetOperation *set_operation;
        SqlSelectStatement *select;
        SqlStatement *stmt;
        SQL_STATEMENT(stmt, set_operation_STATEMENT);
        MALLOC_STATEMENT(stmt, set_operation, SqlSetOperation);
        UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
        set_operation->type = SET_UNION;
        set_operation->operand[0] = $1;
        set_operation->operand[1] = $3;
        UNPACK_SQL_STATEMENT(select, $$, select);
        select->set_operation = stmt;
    }
  | query_expression UNION ALL query_term non_join_query_expression_tail_tail {
        $$ = $1;
        assert(($1)->type == select_STATEMENT);
        assert(($4)->type == select_STATEMENT);
        SqlSetOperation *set_operation;
        SqlSelectStatement *select;
        SqlStatement *stmt;
        SQL_STATEMENT(stmt, set_operation_STATEMENT);
        MALLOC_STATEMENT(stmt, set_operation, SqlSetOperation);
        UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
        set_operation->type = SET_UNION_ALL;
        set_operation->operand[0] = $1;
        set_operation->operand[1] = $4;
        UNPACK_SQL_STATEMENT(select, $$, select);
        select->set_operation = stmt;
    }
  | query_expression EXCEPT query_term {
        $$ = $1;
        SqlSetOperation *set_operation;
        SqlSelectStatement *select;
        SqlStatement *stmt;
        SQL_STATEMENT(stmt, set_operation_STATEMENT);
        MALLOC_STATEMENT(stmt, set_operation, SqlSetOperation);
        UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
        set_operation->type = SET_EXCEPT;
        set_operation->operand[0] = $1;
        set_operation->operand[1] = $3;
        UNPACK_SQL_STATEMENT(select, $$, select);
        select->set_operation = stmt;
    }
  | query_expression EXCEPT ALL query_term non_join_query_expression_tail_tail {
        $$ = $1;
        SqlSetOperation *set_operation;
        SqlSelectStatement *select;
        SqlStatement *stmt;
        SQL_STATEMENT(stmt, set_operation_STATEMENT);
        MALLOC_STATEMENT(stmt, set_operation, SqlSetOperation);
        UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
        set_operation->type = SET_EXCEPT_ALL;
        set_operation->operand[0] = $1;
        set_operation->operand[1] = $4;
        UNPACK_SQL_STATEMENT(select, $$, select);
        select->set_operation = stmt;
    }
  ;

non_join_query_expression_tail_tail
  : /* Empty */ { $$ = NULL; }
  | CORRESPONDING non_join_query_expression_tail_tail_tail
  ;

non_join_query_expression_tail_tail_tail
  : /* Empty */ { $$ = NULL; }
  | BY LEFT_PAREN corresponding_column_list RIGHT_PAREN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "non_join_query_expression_tail_tail_tail: BY LEFT_PAREN corresponding_column_list RIGHT_PAREN"); YYABORT; }
  ;

corresponding_column_list
  : column_name_list { $$ = $1; }
  ;

column_name_list
  : column_name column_name_list_tail { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "column_name_list: column_name column_name_list_tail"); YYABORT; }
  ;

column_name_list_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA column_name_list { $$ = $2; }
  ;

query_term
  : non_join_query_term { $$ = $1; }
  | joined_table { $$ = $1; }
  ;

non_join_query_term
  : non_join_query_primary {$$ = $1; }
  | query_term INTERSECT corresponding_spec query_primary {
        $$ = $1;
        SqlSetOperation *set_operation;
        SqlSelectStatement *select;
        SqlStatement *stmt;
        SQL_STATEMENT(stmt, set_operation_STATEMENT);
        MALLOC_STATEMENT(stmt, set_operation, SqlSetOperation);
        UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
        set_operation->type = SET_INTERSECT;
        set_operation->operand[0] = $1;
        set_operation->operand[1] = $4;
        UNPACK_SQL_STATEMENT(select, $$, select);
        select->set_operation = stmt;
    }
  | query_term INTERSECT ALL corresponding_spec query_primary {
        $$ = $1;
        SqlSetOperation *set_operation;
        SqlSelectStatement *select;
        SqlStatement *stmt;
        SQL_STATEMENT(stmt, set_operation_STATEMENT);
        MALLOC_STATEMENT(stmt, set_operation, SqlSetOperation);
        UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
        set_operation->type = SET_INTERSECT_ALL;
        set_operation->operand[0] = $1;
        set_operation->operand[1] = $5;
        UNPACK_SQL_STATEMENT(select, $$, select);
        select->set_operation = stmt;
    }
  ;

corresponding_spec
  : CORRESPONDING corresponding_spec_tail { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "corresponding_spec: CORRESPONDING corresponding_spec_tail"); YYABORT; }
  | /* Empty */
  ;

corresponding_spec_tail
  : BY LEFT_PAREN corresponding_column_list RIGHT_PAREN { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "corresponding_spec_tail: BY LEFT_PAREN corresponding_column_list RIGHT_PAREN"); YYABORT; }
  ;

non_join_query_primary
  : simple_table {$$ = $1; }
  | LEFT_PAREN non_join_query_expression RIGHT_PAREN { $$ = $2; }
  ;

simple_table
  : table_value_constructor { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "table_value_constructor"); YYABORT; }
  | explicit_table { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "explicit_table"); YYABORT; }
  | sql_select_statement { $$ = $1; }
  ;

table_value_constructor
  : VALUES table_value_constructor_list { $$ = $2; }
  ;

table_value_constructor_list
  : row_value_constructor table_value_constructor_list_tail { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "column_name_list: column_name column_name_list_tail"); YYABORT; }
  ;

table_value_constructor_list_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA table_value_constructor_list { $$ = $2; }
  ;

explicit_table
  : TABLE column_name { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "corresponding_spec_tail: BY LEFT_PAREN corresponding_column_list RIGHT_PAREN"); YYABORT; }
  ;

query_primary
  : non_join_query_primary { $$ = $1;}
  | joined_table { $$ = $1;}
  ;

sql_schema_statement
  : sql_schema_definition_statement { $$ = $1; }
  | sql_schema_manipulation_statement { $$ = $1;}
  ;

/// TODO: not complete
sql_schema_manipulation_statement
  : drop_table_statement { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "sql_schema_manipulation_statement: drop_table_statement"); YYABORT; }
  ;

sql_schema_definition_statement
  : table_definition { $$ = $1; }
  ;

/// TODO: not complete
table_definition
  : CREATE TABLE column_name LEFT_PAREN table_element_list RIGHT_PAREN table_definition_tail {
        SQL_STATEMENT($$, table_STATEMENT);
        MALLOC_STATEMENT($$, table, SqlTable);
        memset(($$)->v.table, 0, sizeof(SqlTable));
        assert($column_name->type == value_STATEMENT
          && $column_name->v.value->type == COLUMN_REFERENCE);
        ($$)->v.table->tableName = $column_name;
        ($$)->v.table->columns = $table_element_list;
        assign_table_to_columns($$);
        if(create_table_defaults($$, $table_definition_tail)) {
          YYABORT;
        }
        dqinit(($$)->v.table);
      }
  ;

table_definition_tail
  : /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
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
    }
  ;

optional_keyword_element
  : GLOBAL literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = OPTIONAL_SOURCE;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
    }
  | DELIM literal_value {
       SQL_STATEMENT($$, keyword_STATEMENT);
        MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_DELIM;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);
     }
  ;

optional_keyword_tail
  : /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | optional_keyword { assert($optional_keyword->type == keyword_STATEMENT); $$ = $optional_keyword; }
  ;

table_element_list
  :  table_element table_element_list_tail  {
      $$ = $table_element;
      assert($$->type == column_STATEMENT);
      if($table_element_list_tail)
      {
        SqlColumn *t_column;
        assert($table_element_list_tail->type == column_STATEMENT);
        dqinsert($table_element_list_tail->v.column, ($$)->v.column, t_column);
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
      MALLOC_STATEMENT($$, column, SqlColumn);
      dqinit(($$)->v.column);
      ($$)->v.column->columnName = $column_name;
      assert($data_type->type == data_type_STATEMENT);
      ($$)->v.column->type = $data_type->v.data_type;
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
    }
  | START literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_START;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $3, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
    }
  | END literal_value column_definition_tail {
       SQL_STATEMENT($$, keyword_STATEMENT);
       MALLOC_STATEMENT($$, keyword, SqlOptionalKeyword);
       ($$)->v.keyword->keyword = OPTIONAL_END;
       ($$)->v.keyword->v = $2;
       dqinit(($$)->v.keyword);

       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $3, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
    }
  | column_constraint_definition column_definition_tail {
       $$ = $column_constraint_definition;
       SqlOptionalKeyword *keyword, *t_keyword;
       UNPACK_SQL_STATEMENT(keyword, $2, keyword);
       dqinsert(keyword, ($$)->v.keyword, t_keyword);
    }
  ;

column_constraint_definition
  : constraint_name_definition column_constraint constraint_attributes {
      ($$) = $column_constraint;
    }
  ;

/// TODO: not complete
constraint_name_definition
  : /* Empty */ { $$ = NULL; }
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
  : qualified_identifier { $$ = $1; }
//  | schema_name period qualified_identifier
  ;

qualified_identifier
  : identifier { $$ = $1; }
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
    }
//  | character_string_type CHARACTER SET character_set_specification
//  | national_character_string_type
//  | bit_string_type
  | numeric_type {
      SQL_STATEMENT($$, data_type_STATEMENT);
      ($$)->v.data_type = INTEGER_TYPE;
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
