/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

insert_statement
  : INSERT INTO qualified_identifier query_expression {
	$$ = insert_statement($qualified_identifier, NULL, NULL, $query_expression, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | INSERT INTO qualified_identifier optional_insert_words query_expression {
	$$ = insert_statement($qualified_identifier, NULL, $optional_insert_words, $query_expression, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | INSERT INTO qualified_identifier LEFT_PAREN column_name_list RIGHT_PAREN query_expression {
	$$ = insert_statement($qualified_identifier, $column_name_list, NULL, $query_expression, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | INSERT INTO qualified_identifier LEFT_PAREN column_name_list RIGHT_PAREN optional_insert_words query_expression {
	$$ = insert_statement($qualified_identifier, $column_name_list, $optional_insert_words, $query_expression, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | INSERT INTO qualified_identifier DEFAULT VALUES {
 	ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "INSERT INTO qualified_identifier DEFAULT VALUES");
	YYABORT;
	/* TODO: YDBOcto#555 : Uncomment below when "INSERT INTO table DEFAULT VALUES" functionality is implemented.
	$$ = insert_statement($qualified_identifier, NULL, NULL, plan_id, parse_context);
	if (NULL == $$) {
		YYERROR;
	}
	*/
    }
  ;

optional_insert_words
  : OVERRIDING_SYSTEM_VALUE {
	MALLOC_KEYWORD_STMT($$, OPTIONAL_OVERRIDING_SYSTEM_VALUE);
    }
  | OVERRIDING_USER_VALUE {
	MALLOC_KEYWORD_STMT($$, OPTIONAL_OVERRIDING_USER_VALUE);
    }
  ;
