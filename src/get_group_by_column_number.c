/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/
#include <assert.h>
#include <stdlib.h>

#include "octo.h"
#include "octo_types.h"
#include "memory_chunk.h"

/* Find if any GROUP BY column `stmt` match `stmt_to_match`.
 * Return GroupBy list node/column number of the matching `stmt`.
 * Returns -1 if no matching is found.
 */
int get_group_by_column_number(SqlTableAlias *table_alias, SqlStatement *stmt_to_match) {
	assert(column_alias_STATEMENT != stmt_to_match->type);
	// Get GroupBy expression list
	SqlSelectStatement *select;
	UNPACK_SQL_STATEMENT(select, table_alias->table, select);
	SqlStatement *group_by_expression = select->group_by_expression;
	if (NULL == group_by_expression)
		return -1;

	// See if there is match between GROUP BY column and `stmt_to_match`
	int		    column_number = 1;
	SqlColumnList	   *cl;
	SqlColumnListAlias *cur_cla, *start_cla;
	group_by_fields_t  *gb_fields;
	UNPACK_SQL_STATEMENT(start_cla, group_by_expression, column_list_alias);
	cur_cla = start_cla;
	do {
		UNPACK_SQL_STATEMENT(cl, cur_cla->column_list, column_list);
		gb_fields = get_group_by_fields(cl->value);
		if (NULL != gb_fields) {
			assert(column_alias_STATEMENT != cl->value->type);
			/* Column aliases are handled by qualify_statement().
			 * For example, if `id` is a column and its used at different clauses
			 * including GROUP BY then qualify_statement() would have set its group_by_column_number. When its set,
			 * this information is available in all `id` references because qualify_column_name() takes care
			 * to set the same column references with pointer to the same SqlColumnAlias. As a result all information
			 * updated to one is carried over to others. Since its handled in such a way we do not want to use
			 * expression matching logic on them. Hence we avoid processing column_alias_STATEMENT here and we do not
			 * expect the stmt_to_match to be of type column_alias_STATEMENT as well. Also even if an expression has a
			 * column reference and the matching element in GROUP BY is the reference itself, we do not have to worry
			 * about it because the GROUP BY information of the column reference is propogated to the reference in the
			 * expression by the way explained in this comment.
			 */
			if (TRUE == match_sql_statement(stmt_to_match, cl->value)) {
				return column_number;
			}
			// We don't have a match. Keep looking.
		}
		column_number++;
		cur_cla = cur_cla->next;
	} while (cur_cla != start_cla);
	return -1;
}
