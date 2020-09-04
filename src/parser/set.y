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

sql_set_statement
  : SET identifier EQUALS literal_value {
      $$ = sql_set_statement($identifier, $literal_value, parse_context);
    }
  | SET identifier TO literal_value {
      $$ = sql_set_statement($identifier, $literal_value, parse_context);
    }
  | SET identifier EQUALS identifier {
      $$ = sql_set_statement($2, $4, parse_context);
    }
  | SET identifier TO identifier {
      $$ = sql_set_statement($2, $4, parse_context);
    }
  | SHOW identifier {
      if (TRUE == parse_context->is_extended_query) {
	parse_context->command_tag = show_STATEMENT;
      }
      SqlShowStatement *show;
      SQL_STATEMENT($$, show_STATEMENT);
      MALLOC_STATEMENT($$, show, SqlShowStatement);
      UNPACK_SQL_STATEMENT(show, $$, show);
      show->variable = $identifier;
    }
  ;
