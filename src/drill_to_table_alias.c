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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

SqlStatement *drill_to_table_alias(SqlStatement *sqlStmt)
{
	for ( ; table_alias_STATEMENT != sqlStmt->type; )
	{
		assert(set_operation_STATEMENT == sqlStmt->type);
		// Fetch operand[0] to determine the list of column names of the final set operation
		sqlStmt = sqlStmt->v.set_operation[0].operand[0];
		assert(NULL != sqlStmt);
	}
	return sqlStmt;
}
