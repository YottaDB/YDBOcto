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

drop_table_statement
  : DROP TABLE column_name DROP_BEHAVIOR {
      SQL_STATEMENT($$, drop_STATEMENT);
      ($$)->v.drop = (SqlDropStatement*)octo_cmalloc(memory_chunks, sizeof(SqlDropStatement));
      ($$)->v.drop->table_name = $column_name;
      ($$)->v.drop->optional_keyword = $DROP_BEHAVIOR;
    }
  ;

DROP_BEHAVIOR
  : /* Empty */ { $$ = NULL; }
  | CASCADE {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)octo_cmalloc(memory_chunks, sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_CASCADE;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | RESTRICT {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)octo_cmalloc(memory_chunks, sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_RESTRICT;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  ;
