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
  : DROP TABLE column_name DROP_BEHAVIOR {
      SQL_STATEMENT($$, drop_table_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.drop_table, SqlDropTableStatement);
      ($$)->v.drop_table->table_name = $column_name;
      ($$)->v.drop_table->optional_keyword = $DROP_BEHAVIOR;
      ($$)->v.drop_table->if_exists_specified = FALSE;
    }
  | DROP TABLE IF EXISTS column_name DROP_BEHAVIOR {
      SQL_STATEMENT($$, drop_table_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.drop_table, SqlDropTableStatement);
      ($$)->v.drop_table->table_name = $column_name;
      ($$)->v.drop_table->optional_keyword = $DROP_BEHAVIOR;
      ($$)->v.drop_table->if_exists_specified = TRUE;
    }
  ;

drop_function_statement
  : DROP FUNCTION IDENTIFIER_START {
      SQL_STATEMENT($$, drop_function_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.drop_function, SqlDropFunctionStatement);
      ($$)->v.drop_function->function_name = $IDENTIFIER_START;
      ($$)->v.drop_function->function_name->v.value->type = FUNCTION_NAME;
      ($$)->v.drop_function->parameter_type_list = NULL;
    }
  | DROP FUNCTION IDENTIFIER_START LEFT_PAREN function_parameter_type_list RIGHT_PAREN {
      SQL_STATEMENT($$, drop_function_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.drop_function, SqlDropFunctionStatement);
      ($$)->v.drop_function->function_name = $IDENTIFIER_START;
      ($$)->v.drop_function->function_name->v.value->type = FUNCTION_NAME;
      ($$)->v.drop_function->parameter_type_list = $function_parameter_type_list;
    }
  ;

DROP_BEHAVIOR
  : /* Empty */ { $$ = NULL; }
  | CASCADE {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = OPTIONAL_CASCADE;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | RESTRICT {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = OPTIONAL_RESTRICT;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  ;
