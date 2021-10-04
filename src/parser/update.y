/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

update_statement_searched
  : UPDATE column_name SET set_clause_list {
      $$ = update_statement($column_name, NULL, $set_clause_list, NULL, plan_id, parse_context);
      if (NULL == $$) {
        YYERROR;
      }
    }
  | UPDATE column_name optional_as as_name SET set_clause_list {
      $$ = update_statement($column_name, $as_name, $set_clause_list, NULL, plan_id, parse_context);
      if (NULL == $$) {
        YYERROR;
      }
    }
  | UPDATE column_name SET set_clause_list WHERE search_condition {
      $$ = update_statement($column_name, NULL, $set_clause_list, $search_condition, plan_id, parse_context);
      if (NULL == $$) {
        YYERROR;
      }
    }
  | UPDATE column_name optional_as as_name SET set_clause_list WHERE search_condition {
      $$ = update_statement($column_name, $as_name, $set_clause_list, $search_condition, plan_id, parse_context);
      if (NULL == $$) {
        YYERROR;
      }
    }
  ;

set_clause_list
  : set_clause set_clause_tail	{
      SqlUpdateColumnValue *colvalue2;

      colvalue2 = (SqlUpdateColumnValue *)$set_clause_tail;
      if (NULL != colvalue2) {
        SqlUpdateColumnValue *colvalue1;

        colvalue1 = (SqlUpdateColumnValue *)$set_clause;
        dqappend(colvalue1, colvalue2);
      }
      $$ = $set_clause;
    }
  ;

set_clause_tail
  : /* Empty */			{ $$ = NULL; }
  | COMMA set_clause_list	{ $$ = $set_clause_list; }
  ;

set_clause
  : object_column EQUALS row_value_constructor_element {
      SqlUpdateColumnValue *colvalue;

      OCTO_CMALLOC_STRUCT(colvalue, SqlUpdateColumnValue);
      dqinit(colvalue);
      colvalue->col_name = $object_column;
      colvalue->col_value = $row_value_constructor_element;
      $$ = (SqlStatement *)colvalue;	/* Needed as all grammar rules need to return "SqlStatement *".
      					 * Caller knows to type cast it back to a "SqlUpdateColumnValue *".
					 */
    }
  ;

object_column
  : column_name
  ;
