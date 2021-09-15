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

drop_table_statement
  : DROP TABLE column_name drop_behavior {
      INVOKE_DROP_TABLE_STATEMENT($$, $column_name, $drop_behavior, FALSE);
    }
  | DROP TABLE IF EXISTS column_name drop_behavior {
      INVOKE_DROP_TABLE_STATEMENT($$, $column_name, $drop_behavior, TRUE);
    }
  ;

drop_function_statement
  : DROP FUNCTION identifier_start optional_function_parameter_type_list {
	INVOKE_DROP_FUNCTION($$, $identifier_start, $optional_function_parameter_type_list, FALSE);
    }
  | DROP FUNCTION IF EXISTS identifier_start optional_function_parameter_type_list {
	INVOKE_DROP_FUNCTION($$, $identifier_start, $optional_function_parameter_type_list, TRUE);
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
