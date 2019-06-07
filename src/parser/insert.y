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

insert_statement
  : INSERT INTO column_name subquery { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "insert_statement: INSERT INTO column_name subquery"); YYABORT; }
  | INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "insert_statement: INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression"); YYABORT; }
  | INSERT INTO column_name DEFAULT VALUES { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "insert_statement: INSERT INTO column_name DEFAULT VALUES"); YYABORT; }
  ;
