/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

insert_statement
  : INSERT INTO column_name query_expression {
	$$ = insert_statement($column_name, NULL, $query_expression, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression {
	$$ = insert_statement($column_name, $column_name_list, $query_expression, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | INSERT INTO column_name DEFAULT VALUES {
 	ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "INSERT INTO column_name DEFAULT VALUES");
	YYABORT;
	/* TODO: YDBOcto#502 : Uncomment below when "INSERT INTO table DEFAULT VALUES" functionality is implemented.
	$$ = insert_statement($column_name, NULL, NULL, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
	*/
    }
  ;
