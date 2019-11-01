/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

%require "3.0"
%debug

%code requires
{
  #include "physical-parser.h"
  #define YYDEBUG 1
  typedef struct Expr Expr;

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
  : START_EXPR LITERAL END_EXPR expression { $$ = $LITERAL; ($$)->next = $expression; parser_value = ($$); YYACCEPT; }

expression
  : expression_value expression_tail {
      $$ = $expression_value;
      ($$)->next = $expression_tail;
    }
  | ENDOFFILE { $$ = NULL; }
  ;

expression_value
  : LITERAL { $$ = $LITERAL; }
  | START_EXPR LITERAL END_EXPR { $$ = $LITERAL; }
  | START_VALUE LITERAL END_VALUE { $$ = $LITERAL; }
  ;

expression_tail
  : expression { $$ = $expression; }
  ;
