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

SqlTable *find_table(const char *table_name) {
	SqlValue *tmp_value;
	SqlTable *start_table, *cur_table;

	TRACE(CUSTOM_ERROR, "Searching for table %s",
	      table_name);

	if(definedTables == NULL)
		return NULL;
	start_table = cur_table = definedTables;
	do {
		UNPACK_SQL_STATEMENT(tmp_value, cur_table->tableName, value);
		if(strcmp(tmp_value->v.reference, table_name) == 0) {
			return cur_table;
		}
		cur_table = cur_table->next;
	} while(start_table != cur_table);
	return NULL;
}
