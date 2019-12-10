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

// Returns the Nth cla (from the SELECT column list) in the input table_alias
// Returns NULL if N is outside of the valid range (e.g. if table has 3 columns, valid range is 1,2,3)
SqlColumnListAlias *get_column_list_alias_n_from_table_alias(SqlTableAlias *table_alias, int column_number) {
	SqlColumnListAlias	*start_cla, *cur_cla;

	UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
	cur_cla = start_cla;
	assert(0 <= column_number);
	if (0 == column_number) {
		/* Input column number is less than the # of available columns */
		return NULL;
	}
	do {
		if (0 == --column_number) {
			break;
		}
		cur_cla = cur_cla->next;
	} while (cur_cla != start_cla);
	if (column_number) {
		/* Input column number is more than the # of available columns */
		return NULL;
	}
	return cur_cla;
}
