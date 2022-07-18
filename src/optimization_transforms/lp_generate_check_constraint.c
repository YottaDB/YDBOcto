/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "octo.h"
#include "logical_plan.h"

/* Check if there are any table-level and/or column-level constraints.
 * If so, convert them into LP_CHECK_CONSTRAINT logical plans.
 */
boolean_t lp_generate_check_constraint(LogicalPlan **lp_constraint_ptr, SqlStatement *stmt, SqlTableAlias *table_alias) {
	SqlColumn *  cur_column, *start_column;
	SqlTable *   table;
	LogicalPlan *ret;
	boolean_t    error_encountered = FALSE;

	assert((insert_STATEMENT == stmt->type) || (update_STATEMENT == stmt->type));
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		SqlOptionalKeyword *cur_keyword, *start_keyword;

		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			cur_keyword = cur_keyword->next;
			if (OPTIONAL_CHECK_CONSTRAINT == cur_keyword->keyword) {
				SqlConstraint *constraint;
				LogicalPlan *  lp_constraint, *lp_where;

				MALLOC_LP_2ARGS(lp_constraint, LP_CHECK_CONSTRAINT);
				MALLOC_LP(lp_where, lp_constraint->v.lp_default.operand[0], LP_WHERE);
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				lp_constraint->extra_detail.lp_check_constraint.constraint = constraint;
				LP_GENERATE_WHERE(constraint->definition, stmt, stmt, ret, error_encountered);
				lp_where->v.lp_default.operand[0] = ret;
				*lp_constraint_ptr = lp_constraint;
				if (NULL != ret) {
					lp_constraint_ptr = &lp_constraint->v.lp_default.operand[1];
				}
			}
		} while (cur_keyword != start_keyword);
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	return error_encountered;
}
