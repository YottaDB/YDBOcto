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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

int tables_equal(SqlTable *a, SqlTable *b) {
	SqlValue *valA, *valB;
	UNPACK_SQL_STATEMENT(valA, a->tableName, value);
	UNPACK_SQL_STATEMENT(valB, b->tableName, value);
	if(values_equal(valA, valB) == FALSE)
		return FALSE;
	// But we won't test columns, as that would be somewhat expensive
	return TRUE;
}
