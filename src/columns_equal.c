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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

int columns_equal(SqlColumn *a, SqlColumn *b) {
	SqlValue *valA, *valB;
	SqlTable *tableA, *tableB;

	if (memcmp(&a->data_type_struct, &b->data_type_struct, sizeof(a->data_type_struct)))
		return FALSE;
	UNPACK_SQL_STATEMENT(valA, a->columnName, value);
	UNPACK_SQL_STATEMENT(valB, b->columnName, value);
	if (values_equal(valA, valB) == FALSE)
		return FALSE;
	UNPACK_SQL_STATEMENT(tableA, a->table, create_table);
	UNPACK_SQL_STATEMENT(tableB, b->table, create_table);
	if (tables_equal(tableA, tableB) == FALSE)
		return FALSE;
	// Note, we don't compare keywords because it is highly unlikely that they would
	//  be different, and that would be a bad state, but expensive to do
	//  It could be added as a debug check
	return TRUE;
}
