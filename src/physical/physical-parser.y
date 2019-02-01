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

%code requires
{
  #include "physical-parser.h"
  #define YYDEBUG 1
  struct Expr typedef Expr;

  enum ExprType {
    LITERAL_TYPE,
    EXPR_TYPE,
    VALUE_TYPE
  };

// Tree should be organized as EXPR -> LITERAL -> ARGUMENT -> ARGUMENT -> EXPR ...  

  struct Expr {
    int type;
    char *value;
    Expr *next;
  };
  typedef struct Expr * YYSTYPE;

  extern struct Expr *parser_value;
  extern int yylex(YYSTYPE * yylval_param);
}

%{
  #include <stdio.h>
  #include <stdlib.h>

  #include <config.h>
  #include <errors.h>


  void yyerror(char const *s) {
    ERROR(CUSTOM_ERROR, "%s", s);
  }

  struct Expr *parser_value;
%}

%define api.pure full
%define api.value.type {struct Expr *}

%token START_EXPR
%token END_EXPR
%token START_VALUE
%token END_VALUE

%token ENDOFFILE
%token LITERAL

%%

template_statement
  : START_EXPR LITERAL END_EXPR expression { $$ = $2; ($$)->next = $4; parser_value = ($$); YYACCEPT; }

expression
  : expression_value expression_tail { 
      $$ = $1;
      ($$)->next = $2;
    }
  | ENDOFFILE { $$ = NULL; }
  ;

expression_value
  : LITERAL { $$ = $1; }
  | START_EXPR LITERAL END_EXPR { $$ = $2; }
  | START_VALUE LITERAL END_VALUE { $$ = $2; }
  ;

expression_tail
  : expression { $$ = $1; }
  ;
