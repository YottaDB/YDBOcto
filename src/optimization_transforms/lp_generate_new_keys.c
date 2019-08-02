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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_replace_helper(LogicalPlan *where, SqlTableAlias *table_alias, SqlKey *key);

/**
 * Goes through plan and replaces all key unique id's (and references to those keys) with a new, unique key
 */
LogicalPlan *lp_generate_new_keys(LogicalPlan *root, LogicalPlan *new_plan) {
	SqlKey *key;
	LogicalPlan *key_lp, *t;

	key_lp = lp_get_output_key(new_plan);
	key = key_lp->v.key;
	t = lp_get_select_where(root);
	t->v.operand[0] = lp_replace_helper(t->v.operand[0], table_alias, key);
	t = lp_get_project(root);
	t->v.operand[0] = lp_replace_helper(t->v.operand[0], table_alias, key);

	return root;
}

LogicalPlan *lp_replace_helper(LogicalPlan *where, SqlTableAlias *table_alias, int new_unique_id) {
	SqlColumn *column;
	SqlColumnAlias *alias;
	SqlValue *value;
	SqlColumnListAlias *cur_cl_alias, *start_cl_alias, *t_cl_alias;
	SqlTableAlias *t_table_alias;
	LogicalPlan *ret = where;
	char *column_name1, *column_name2;
	int part = 1;

	if(where == NULL)
		return NULL;

	switch(where->type) {
	case LP_COLUMN_ALIAS:
		alias = where->v.column_alias;
		if(alias->table_alias->v.table_alias == table_alias) {
			MALLOC_LP_2ARGS(ret, LP_DERIVED_COLUMN);
			MALLOC_LP_2ARGS(ret->v.operand[0], LP_KEY);
			ret->v.operand[0]->v.key = key;
			part = get_column_piece_number(alias, table_alias);
			MALLOC_LP_2ARGS(ret->v.operand[1], LP_PIECE_NUMBER);
			ret->v.operand[1]->v.piece_number = part;
		}
		break;
	case LP_COLUMN_LIST:
		ret->v.operand[0] = lp_replace_helper(where->v.operand[0], table_alias, key);
		ret->v.operand[1] = lp_replace_helper(where->v.operand[1], table_alias, key);
		break;
	case LP_WHERE:
		ret->v.operand[0] = lp_replace_helper(where->v.operand[0], table_alias, key);
		break;
	case LP_SET_OPERATION:
		// I don't expect this to happen, but seems like a possible path
		assert(FALSE);
		break;
	default:
		if(where->type > LP_ADDITION) {
			ret->v.operand[0] = lp_replace_helper(where->v.operand[0], table_alias, key);
			ret->v.operand[1] = lp_replace_helper(where->v.operand[1], table_alias, key);
		}
		break;
	}
	return ret;
}
