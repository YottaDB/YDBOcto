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

sql_set_statement
  : SET identifier EQUALS literal_value {
      SqlSetStatement *set;
      SQL_STATEMENT($$, set_STATEMENT);
      MALLOC_STATEMENT($$, set, SqlSetStatement);
      UNPACK_SQL_STATEMENT(set, $$, set);
      set->variable = $identifier;
      set->value = $literal_value;
    }
  | SET identifier TO literal_value {
      SqlSetStatement *set;
      SQL_STATEMENT($$, set_STATEMENT);
      MALLOC_STATEMENT($$, set, SqlSetStatement);
      UNPACK_SQL_STATEMENT(set, $$, set);
      set->variable = $identifier;
      set->value = $literal_value;
    }
  | SHOW identifier {
      SqlShowStatement *show;
      SQL_STATEMENT($$, show_STATEMENT);
      MALLOC_STATEMENT($$, show, SqlShowStatement);
      UNPACK_SQL_STATEMENT(show, $$, show);
      show->variable = $identifier;
    }
  ;
