/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Given a "table" parameter, this function returns TRUE if the table has a hidden column (HIDDEN_KEY_COL_NAME).
 * And FALSE otherwise.
 */
boolean_t table_has_hidden_column(SqlTable *table) {
	SqlColumn *cur_column, *start_column;

	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		if (cur_column->is_hidden_keycol) {
			return TRUE;
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	return FALSE;
}
