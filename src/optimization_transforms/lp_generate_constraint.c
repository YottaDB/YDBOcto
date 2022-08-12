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
 * If so, convert them into LP_CHECK_CONSTRAINT/LP_UNIQUE_CONSTRAINT etc. logical plans.
 */
boolean_t lp_generate_constraint(LogicalPlan **lp_constraint_ptr, SqlStatement *stmt, SqlTableAlias *table_alias) {
	SqlColumn *   cur_column, *start_column;
	SqlTable *    table;
	LogicalPlan * ret;
	boolean_t     error_encountered = FALSE;
	LogicalPlan * lp_constraint;
	LogicalPlan **lp_check_constraint_ptr, **lp_unique_constraint_ptr;

	lp_constraint = NULL;
	lp_check_constraint_ptr = lp_unique_constraint_ptr = NULL; /* to avoid false [-Wmaybe-uninitialized] warning */
	assert((insert_STATEMENT == stmt->type) || (update_STATEMENT == stmt->type) || (delete_from_STATEMENT == stmt->type));
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		SqlOptionalKeyword *cur_keyword, *start_keyword;

		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			cur_keyword = cur_keyword->next;
			switch (cur_keyword->keyword) {
			case OPTIONAL_CHECK_CONSTRAINT:
			case UNIQUE_CONSTRAINT:;
				SqlConstraint *constraint;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);

				if ((OPTIONAL_CHECK_CONSTRAINT == cur_keyword->keyword) && (delete_from_STATEMENT == stmt->type)) {
					/* No need to do anything for CHECK constraints in case of a DELETE FROM */
					break;
				}
				if (NULL == lp_constraint) {
					MALLOC_LP_2ARGS(lp_constraint, LP_CONSTRAINT);
					lp_check_constraint_ptr = &lp_constraint->v.lp_default.operand[0];
					lp_unique_constraint_ptr = &lp_constraint->v.lp_default.operand[1];
					*lp_constraint_ptr = lp_constraint;
				}
				if (OPTIONAL_CHECK_CONSTRAINT == cur_keyword->keyword) {
					LogicalPlan *lp_check_constraint, *lp_where;
					MALLOC_LP_2ARGS(lp_check_constraint, LP_CHECK_CONSTRAINT);
					*lp_check_constraint_ptr = lp_check_constraint;
					MALLOC_LP(lp_where, lp_check_constraint->v.lp_default.operand[0], LP_WHERE);
					lp_check_constraint->extra_detail.lp_constraint.constraint = constraint;
					LP_GENERATE_WHERE(constraint->definition, stmt, ret, error_encountered);
					lp_where->v.lp_default.operand[0] = ret;
					if (NULL != ret) {
						lp_check_constraint_ptr = &lp_check_constraint->v.lp_default.operand[1];
					}
				} else {
					assert(UNIQUE_CONSTRAINT == cur_keyword->keyword);

					LogicalPlan *lp_unique_constraint;
					MALLOC_LP_2ARGS(lp_unique_constraint, LP_UNIQUE_CONSTRAINT);
					lp_unique_constraint->extra_detail.lp_constraint.constraint = constraint;
					*lp_unique_constraint_ptr = lp_unique_constraint;

					SqlColumnList *start_cl;
					UNPACK_SQL_STATEMENT(start_cl, constraint->definition, column_list);
					error_encountered |= lp_generate_column_list(&lp_unique_constraint->v.lp_default.operand[0],
										     NULL, start_cl);
					if (error_encountered) {
						break;
					}
					lp_unique_constraint_ptr = &lp_unique_constraint->v.lp_default.operand[1];
				}
				break;
			default:
				break;
			}
			if (error_encountered) {
				break;
			}
		} while (cur_keyword != start_keyword);
		if (error_encountered) {
			break;
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	return error_encountered;
}
