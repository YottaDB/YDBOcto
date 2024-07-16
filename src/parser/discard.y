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
      SQL_STATEMENT($$, discard_xrefs_STATEMENT);
    }
  ;

