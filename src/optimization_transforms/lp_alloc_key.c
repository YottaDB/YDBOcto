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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_alloc_key(SqlTable *table, SqlColumn *column, int unique_id, LPActionType type, SqlKey *cross_reference_output_key,
			  boolean_t is_cross_reference_key) {
	LogicalPlan *ret;
	SqlKey *     key;

	MALLOC_LP_2ARGS(ret, LP_KEY);
	OCTO_CMALLOC_STRUCT(key, SqlKey);
	key->table = table;
	key->column = column;
	key->unique_id = unique_id;
	key->type = type;
	key->cross_reference_output_key = cross_reference_output_key;
	key->is_cross_reference_key = is_cross_reference_key;
	ret->v.lp_key.key = key;
	return ret;
}
