/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

delete_statement_searched
  : DELETE FROM qualified_identifier where_clause {
      $$ = delete_from_statement($qualified_identifier, NULL, $where_clause, plan_id, parse_context);
      if (NULL == $$) {
        YYERROR;
      }
    }
  | DELETE FROM qualified_identifier as_name where_clause {
      $$ = delete_from_statement($qualified_identifier, $as_name, $where_clause, plan_id, parse_context);
      if (NULL == $$) {
        YYERROR;
      }
    }
  ;

