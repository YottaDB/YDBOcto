/****************************************************************
 *								*
 * Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

discard_all_statement
  : DISCARD ALL {
      SQL_STATEMENT($$, discard_all_STATEMENT);
    }
  ;

discard_xrefs_statement
  : DISCARD XREFS {
      SqlStatement *ret;
      SQL_STATEMENT(ret, discard_xrefs_STATEMENT);
      MALLOC_STATEMENT(ret, discard_xrefs, SqlDiscardXrefs);

      SqlDiscardXrefs *discard_xrefs;
      UNPACK_SQL_STATEMENT(discard_xrefs, ret, discard_xrefs)
      discard_xrefs->type = DISCARD_XREFS_ALL;
      $$ = ret;
    }
  | DISCARD XREFS column_name {
      SqlStatement *ret;
      SQL_STATEMENT(ret, discard_xrefs_STATEMENT);
      MALLOC_STATEMENT(ret, discard_xrefs, SqlDiscardXrefs);

      SqlDiscardXrefs *discard_xrefs;
      UNPACK_SQL_STATEMENT(discard_xrefs, ret, discard_xrefs)
      discard_xrefs->type = DISCARD_XREFS_TABLE;
      discard_xrefs->table_name = $column_name;
      $$ = ret;
    }
  ;

