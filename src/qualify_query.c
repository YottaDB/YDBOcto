/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

int qualify_query(SqlStatement *table_alias_stmt, SqlJoin *parent_join, SqlTableAlias *parent_table_alias) {
	SqlColumnListAlias	*ret_cla;
	SqlJoin			*join;
	SqlJoin			*prev_start, *prev_end;
	SqlJoin			*start_join, *cur_join;
	SqlSelectStatement	*select;
	SqlTableAlias		*table_alias;
	SqlStatement		*group_by_expression;
	int			result = 0;

	if (set_operation_STATEMENT == table_alias_stmt->type) {
		SqlSetOperation		*set_opr;

		UNPACK_SQL_STATEMENT(set_opr, table_alias_stmt, set_operation);
		result |= qualify_query(set_opr->operand[0], parent_join, parent_table_alias);
		result |= qualify_query(set_opr->operand[1], parent_join, parent_table_alias);
		return result;
	}
	assert(table_alias_STATEMENT == table_alias_stmt->type);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
	if (NULL == table_alias->parent_table_alias) {
		table_alias->parent_table_alias = parent_table_alias;
	} else {
		assert(table_alias->parent_table_alias == parent_table_alias);
	}
	if (table_STATEMENT == table_alias->table->type) {
		return result;
	}
	UNPACK_SQL_STATEMENT(select, table_alias->table, select);
	UNPACK_SQL_STATEMENT(join, select->table_list, join);

	/* Ensure strict column name qualification checks (i.e. all column name references have to be a valid column
	 * name in a valid existing table) by using NULL as the last parameter in various `qualify_statement()` calls below.
	 */
	table_alias->do_group_by_checks = FALSE;	/* need to set this before invoking "qualify_statement()" */
	// Qualify FROM clause first
	// Add in joins (if any) from higher/parent level queries so sub-queries (current level) can use them
	prev_start = join;
	prev_end = join->prev;
	if (NULL != parent_join) {
		dqappend(join, parent_join);
	}
	start_join = cur_join = join;
	do {
		SqlJoin		*save_join;

		/* Qualify sub-queries involved in the join. Note that it is possible a `table` is involved in the join instead
		 * of a `sub-query` in which case the below `qualify_query` call will return right away.
		 */
		result |= qualify_query(cur_join->value, NULL, table_alias);
		/* Make sure any table.column references in the ON condition of the JOIN (cur_join->condition) are qualified
		 * until the current table in the join list (i.e. forward references should not be allowed). See YDBOcto#291
		 * for example query. Note that NATURAL JOIN is an exception in that the current join could have table.column
		 * references to one or more later SqlJoin structures in the tablejoin list (`natural_join_condition.c` creates
		 * them when there is more than one NATURAL JOIN in the query). Since there is no ON clause in a NATURAL JOIN,
		 * there is no need to disallow forward references like one needs to in, say, a LEFT JOIN. Therefore
		 * skip this forward-reference-check for a NATURAL JOIN.
		 */
		if (NATURAL_JOIN != cur_join->type)
		{
			save_join = cur_join->next;	/* save join list before tampering with it */
			/* Note that if "parent_join" is non-NULL, we need to include that even though it comes
			 * after all the tables in the join list at the current level. This is so any references
			 * to columns in parent queries are still considered as valid. Hence the parent_join check below.
			 */
			cur_join->next = ((NULL != parent_join) ? parent_join : start_join);	/* stop join list at
												 * current join.
												 */
		} else {
			/* Natural join could have forward references to later tablejoins (if there are later NATURAL JOINs)
			 * and so need to initialize `parent_table_alias` field in all tablejoins before `qualify_statement()`
			 * call below.
			 */
			init_parent_table_alias(table_alias_stmt, table_alias);
		}
		table_alias->aggregate_depth = AGGREGATE_DEPTH_FROM_CLAUSE;
		result |= qualify_statement(cur_join->condition, start_join, table_alias_stmt, 0, NULL);
		if (NATURAL_JOIN != cur_join->type)
			cur_join->next = save_join;	/* restore join list to original */
		cur_join = cur_join->next;
	} while (cur_join != start_join && cur_join != parent_join);
	// Qualify WHERE clause next
	table_alias->aggregate_depth = AGGREGATE_DEPTH_WHERE_CLAUSE;
	result |= qualify_statement(select->where_expression, start_join, table_alias_stmt, 0, NULL);
	// Qualify GROUP BY clause next
	group_by_expression = select->group_by_expression;
	/* Note that while table_alias->aggregate_function_or_group_by_specified will mostly be FALSE at this point, it is
	 * possible for it to be TRUE in some cases (see YDBOcto#457 for example query) if this `qualify_query()` invocation
	 * corresponds to a sub-query in say the HAVING clause of an outer query. In that case, `qualify_query()` for the
	 * sub-query would be invoked twice by the `qualify_query()` of the outer query (see `table_alias->do_group_by_checks`
	 * `for` loop later in this function). If so, we can skip the GROUP BY expression processing for the sub-query the
	 * second time. Hence the `&& !table_alias->aggregate_function_or_group_by_specified)` in the `if` check below.
	 */
	if ((NULL != group_by_expression) && !table_alias->aggregate_function_or_group_by_specified) {
		SqlColumnListAlias	*start_cla, *cur_cla;
		SqlTableAlias		*group_by_table_alias;
		SqlColumnList		*col_list;
		SqlColumnAlias		*column_alias;
		int			group_by_column_count;

		table_alias->aggregate_depth = AGGREGATE_DEPTH_GROUP_BY_CLAUSE;
		assert(0 == table_alias->group_by_column_count);
		result |= qualify_statement(group_by_expression, start_join, table_alias_stmt, 0, NULL);
		/* Note: table_alias->group_by_column_count can still be 0 if GROUP BY was done on a parent query column */
		if (table_alias->group_by_column_count) {
			table_alias->aggregate_function_or_group_by_specified = TRUE;
		}
		/* Traverse the GROUP BY list to see what columns belong to this table_alias. Include only those in the
		 * GROUP BY list. Exclude any other columns (e.g. columns belonging to outer query) from the list
		 * as they are constant as far as this sub-query is concerned.
		 */
		UNPACK_SQL_STATEMENT(start_cla, group_by_expression, column_list_alias);
		group_by_column_count = 0;
		cur_cla = start_cla;
		do {
			UNPACK_SQL_STATEMENT(col_list, cur_cla->column_list, column_list);
			UNPACK_SQL_STATEMENT(column_alias, col_list->value, column_alias);
			UNPACK_SQL_STATEMENT(group_by_table_alias, column_alias->table_alias, table_alias);
			if (group_by_table_alias->parent_table_alias != table_alias) {
				/* Column belongs to an outer query. Discard it from the GROUP BY list. */
				SqlColumnListAlias	*prev, *next;

				prev = cur_cla->prev;
				next = cur_cla->next;
				prev->next = next;
				next->prev = prev;
			} else {
				if (0 == group_by_column_count) {
					group_by_expression->v.column_list_alias = cur_cla;
					start_cla = cur_cla;
				}
				group_by_column_count++;
			}
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);
		assert(group_by_column_count == table_alias->group_by_column_count);
		if (!group_by_column_count) {
			select->group_by_expression = NULL;
		}
	}
	ret_cla = NULL;
	table_alias->aggregate_depth = 0;
	for ( ; ; )
	{
		assert(0 == table_alias->aggregate_depth);
		// Qualify HAVING clause
		result |= qualify_statement(select->having_expression, start_join, table_alias_stmt, 0, NULL);
		// Qualify SELECT column list next
		result |= qualify_statement(select->select_list, start_join, table_alias_stmt, 0, NULL);
		// Qualify ORDER BY clause next
		/* Now that all column names used in the query have been qualified, allow columns specified in
		 * ORDER BY to be qualified against any column names specified till now without any strict checking.
		 * Hence the use of a non-NULL value (`&ret_cla`) as the last parameter to `qualify_statement()` below.
		 */
		result |= qualify_statement(select->order_by_expression, start_join, table_alias_stmt, 0, &ret_cla);
		if (!table_alias->aggregate_function_or_group_by_specified) {
			/* GROUP BY or AGGREGATE function was never used in the query. No need to do GROUP BY validation checks. */
			break;
		}
		if (table_alias->do_group_by_checks) {
			/* GROUP BY or AGGREGATE function was used in the query. And GROUP BY validation checks already done
			 * as part of the second iteration in this for loop. Can now break out of the loop.
			 */
			break;
		}
		if (table_alias->aggregate_function_or_group_by_specified) {
			/* GROUP BY or AGGREGATE function was used in the query. Do GROUP BY validation checks by doing
			 * a second iteration in this for loop.
			 */
			table_alias->do_group_by_checks = TRUE;
			continue;
		}
	}
	/* Make sure to reset parent query only AFTER WHERE clause, ORDER BY clause etc. are processed.
	 * This is because it is possible columns from the current level query can be used in sub-queries
	 * inside the WHERE clause etc. And those column references need the parent query to stay in the
	 * list of joins in order to be qualified correctly (in `qualify_column_name`).
	 */
	if (NULL != parent_join) {
		parent_join->prev = start_join->prev;
		parent_join->prev->next = parent_join;
		start_join->prev = prev_end;
		start_join->prev->next = prev_start;
	}
	return result;
}
