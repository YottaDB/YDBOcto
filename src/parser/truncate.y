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

truncate_table_statement
  : TRUNCATE qualified_table_name_list {
      INVOKE_TRUNCATE_TABLE_STATEMENT($$, $qualified_table_name_list);
    }
  | TRUNCATE TABLE qualified_table_name_list {
      INVOKE_TRUNCATE_TABLE_STATEMENT($$, $qualified_table_name_list);
    }
  ;

