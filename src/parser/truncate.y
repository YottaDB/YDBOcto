/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

truncate_table_statement
  : TRUNCATE truncate_list {
      INVOKE_TRUNCATE_TABLE_STATEMENT($$, $truncate_list);
    }
  | TRUNCATE TABLE truncate_list {
      INVOKE_TRUNCATE_TABLE_STATEMENT($$, $truncate_list);
    }
  ;

truncate_list
  : truncate_sublist truncate_sublist_tail {
	SqlStatement *truncate_list;

	truncate_list = $truncate_sublist;
	/* Process each table in the list recursively through truncate_sublist_tail */
	if (NULL != $truncate_sublist_tail) {
		SqlColumnList	*list1, *list2;

		UNPACK_SQL_STATEMENT(list1, truncate_list, column_list);
		UNPACK_SQL_STATEMENT(list2, $truncate_sublist_tail, column_list);
		dqappend(list1, list2);
	}
	$$ = truncate_list;
    }
  ;

truncate_sublist
  : value_expression {
	$$ = create_sql_column_list($value_expression, NULL, &yyloc);
  }
  ;

truncate_sublist_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA truncate_list { $$ = $truncate_list; }
  ;
