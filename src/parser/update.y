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

update_statement_searched
  : UPDATE column_name SET set_clause_list { $$ = NULL; }
  | UPDATE column_name SET set_clause_list WHERE search_condition { $$ = NULL; }
  ;

set_clause_list
  : set_clause set_clause_tail
  ;

set_clause_tail
  : /* Empty */
  | COMMA set_clause_list
  ;

set_clause
  : object_column EQUALS row_value_constructor_element
  ;

object_column
  : column_name
  ;
