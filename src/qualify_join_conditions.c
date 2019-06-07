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

int qualify_join_conditions(SqlJoin *join, SqlJoin *tables, SqlStatement *column_list_alias) {
	SqlJoin *cur_join, *start_join;
	SqlValueType type;
	int ret = 0;

	cur_join = start_join = join;
	do {
		if(cur_join->condition) {
			ret |= qualify_statement(cur_join->condition, tables, column_list_alias);
			ret |= populate_data_type(cur_join->condition, &type);
		}
		cur_join = cur_join->next;
	} while(cur_join != start_join);
	return ret;
}
