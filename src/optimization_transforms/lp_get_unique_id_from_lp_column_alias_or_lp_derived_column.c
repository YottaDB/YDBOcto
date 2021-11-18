/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
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

/* Returns "unique_id" given a LP_COLUMN_ALIAS or LP_DERIVED_COLUMN logical plan */
int lp_get_unique_id_from_lp_column_alias_or_lp_derived_column(LogicalPlan *plan) {
	int unique_id;

	assert(IS_LP_COLUMN_ALIAS_OR_LP_DERIVED_COLUMN(plan));
	if (LP_COLUMN_ALIAS == plan->type) {
		SqlTableAlias *table_alias;

		UNPACK_SQL_STATEMENT(table_alias, plan->v.lp_column_alias.column_alias->table_alias_stmt, table_alias);
		unique_id = table_alias->unique_id;
	} else {
		assert(LP_DERIVED_COLUMN == plan->type);

		LogicalPlan *key;
		GET_LP(key, plan, 0, LP_KEY);

		SqlKey *sql_key;
		sql_key = key->v.lp_key.key;
		unique_id = sql_key->unique_id;
	}
	assert(0 < unique_id);
	return unique_id;
}
