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

#include <stdlib.h>

#include "logical_plan.h"

LogicalPlan *lp_copy_plan(LogicalPlan *plan) {
	LogicalPlan *new_plan;
	if(plan == NULL)
		return NULL;
	new_plan = (LogicalPlan *)octo_cmalloc(memory_chunks, sizeof(LogicalPlan));
	*new_plan = *plan;
	// We copy SqlStatements where, which is definitely needed for keys
	// and maybe needed for the others, but better safe than sorry
	SqlStatement *stmt;
	switch(plan->type) {
	case LP_KEY:
		new_plan->v.key = lp_copy_key(plan->v.key);
		break;
	case LP_VALUE:
		SQL_STATEMENT(stmt, value_STATEMENT);
		stmt->v.value = plan->v.value;
		new_plan->v.value = copy_sql_statement(stmt)->v.value;
		break;
	case LP_TABLE:
		break;
	case LP_COLUMN_ALIAS:
		SQL_STATEMENT(stmt, column_alias_STATEMENT);
		stmt->v.value = plan->v.value;
		new_plan->v.value = copy_sql_statement(stmt)->v.value;
		break;
	case LP_COLUMN_LIST_ALIAS:
		SQL_STATEMENT(stmt, column_list_alias_STATEMENT);
		stmt->v.value = plan->v.value;
		new_plan->v.value = copy_sql_statement(stmt)->v.value;
		break;
	case LP_KEYWORDS:
		SQL_STATEMENT(stmt, keyword_STATEMENT);
		stmt->v.value = plan->v.value;
		new_plan->v.value = copy_sql_statement(stmt)->v.value;
		break;
	case LP_PIECE_NUMBER:
		break;
	default:
		new_plan->v.operand[0] = lp_copy_plan(plan->v.operand[0]);
		new_plan->v.operand[1] = lp_copy_plan(plan->v.operand[1]);
	}
	return new_plan;
}
