/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

drop_table_statement
  : DROP TABLE qualified_identifier drop_behavior drop_data_retention {
      INVOKE_DROP_TABLE_STATEMENT($$, $qualified_identifier, $drop_behavior, $drop_data_retention, FALSE);
    }
  | DROP TABLE IF EXISTS qualified_identifier drop_behavior drop_data_retention {
      INVOKE_DROP_TABLE_STATEMENT($$, $qualified_identifier, $drop_behavior, $drop_data_retention, TRUE);
    }
  | DROP TABLE sql_identifier drop_behavior drop_data_retention {
      $sql_identifier->v.value->type = COLUMN_REFERENCE;
      INVOKE_DROP_TABLE_STATEMENT($$, $sql_identifier, $drop_behavior, $drop_data_retention, FALSE);
    }
  | DROP TABLE IF EXISTS sql_identifier drop_behavior drop_data_retention {
      $sql_identifier->v.value->type = COLUMN_REFERENCE;
      INVOKE_DROP_TABLE_STATEMENT($$, $sql_identifier, $drop_behavior, $drop_data_retention, TRUE);
    }
  ;

drop_view_statement
  : DROP VIEW column_name {
      INVOKE_DROP_VIEW_STATEMENT($$, $column_name, FALSE);
    }
  | DROP VIEW sql_identifier {
      $sql_identifier->v.value->type = COLUMN_REFERENCE;
      INVOKE_DROP_VIEW_STATEMENT($$, $sql_identifier, FALSE);
    }
  | DROP VIEW IF EXISTS column_name {
      INVOKE_DROP_VIEW_STATEMENT($$, $column_name, TRUE);
    }
  | DROP VIEW IF EXISTS sql_identifier {
      $sql_identifier->v.value->type = COLUMN_REFERENCE;
      INVOKE_DROP_VIEW_STATEMENT($$, $sql_identifier, TRUE);
    }
  ;

drop_function_statement
  : DROP FUNCTION function_name optional_function_parameter_type_list {
	INVOKE_DROP_FUNCTION($$, $function_name, $optional_function_parameter_type_list, FALSE);
    }
  | DROP FUNCTION IF EXISTS function_name optional_function_parameter_type_list {
	INVOKE_DROP_FUNCTION($$, $function_name, $optional_function_parameter_type_list, TRUE);
    }
  | DROP FUNCTION PARENLESS_FUNCTION optional_function_parameter_type_list {
	INVOKE_DROP_FUNCTION($$, $PARENLESS_FUNCTION, $optional_function_parameter_type_list, FALSE);
    }
  | DROP FUNCTION IF EXISTS PARENLESS_FUNCTION optional_function_parameter_type_list {
	INVOKE_DROP_FUNCTION($$, $PARENLESS_FUNCTION, $optional_function_parameter_type_list, TRUE);
    }
  ;

optional_function_parameter_type_list
  : /* Empty */ { $$ = NULL; }
  | LEFT_PAREN function_parameter_type_list RIGHT_PAREN {
      $$ = $function_parameter_type_list;
    }
  ;

drop_behavior
  : /* Empty */ { $$ = NULL; }
  | CASCADE {
      INVOKE_DROP_BEHAVIOR($$, OPTIONAL_CASCADE);
    }
  | RESTRICT {
      INVOKE_DROP_BEHAVIOR($$, OPTIONAL_RESTRICT);
    }
  ;

/* Note: C code using this element should cast it back to type `enum OptionalKeyword` */
drop_data_retention
  : /* Empty */ {
      $$ = (SqlStatement *)NO_KEYWORD;
  }
  | KEEPDATA {
      $$ = (SqlStatement *)OPTIONAL_KEEPDATA;
    }
  ;
