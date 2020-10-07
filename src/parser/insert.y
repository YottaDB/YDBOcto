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
	$$ = insert_statement($column_name, NULL, $query_expression);
    }
  | INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression {
	$$ = insert_statement($column_name, $column_name_list, $query_expression);
    }
  | INSERT INTO column_name DEFAULT VALUES {
	$$ = insert_statement($column_name, NULL, NULL);
    }
  ;
