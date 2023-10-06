/****************************************************************
 *								*
 * Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

sql_dynamic_statement
  : deallocate_prepared_statement { $$ = NULL; }
  ;

/* TODO: Currently the DEALLOCATE keyword is supported minimally only to avoid a parse error.
 * It is a no-op otherwise.
 */
deallocate_prepared_statement
  : DEALLOCATE optional_prepare sql_statement_name { $$ = NULL; }
  ;

statement_name
  : identifier { $$ = $identifier; }
  ;

sql_statement_name
  : statement_name { $$ = $statement_name; }
// | extended_statement_name
  ;

optional_prepare
  : /* Empty */ { $$ = NULL; }
  | PREPARE { $$ = NULL; }	/* https://www.postgresql.org/docs/current/sql-deallocate.html says Postgres ignores
  				 * this optional keyword so we do the same.
				 */
  ;

