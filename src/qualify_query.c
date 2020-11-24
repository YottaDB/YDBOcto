/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

/* Returns:
 *	0 if query is successfully qualified.
 *	1 if query had errors during qualification.
 */
int qualify_query(SqlStatement *table_alias_stmt, SqlJoin *parent_join, SqlTableAlias *parent_table_alias,
		  QualifyStatementParms *ret) {
	SqlColumnListAlias *ret_cla;
	SqlJoin *	    join;
	SqlJoin *	    prev_start, *prev_end;
	SqlJoin *	    start_join, *cur_join;
	SqlSelectStatement *select;
	SqlTableAlias *	    table_alias;
	SqlStatement *	    group_by_expression;
	int		    result;
	SqlStatementType    table_type;
	SqlTableValue *	    table_value;
	SqlRowValue *	    row_value, *start_row_value;

	result = 0;
	if (insert_STATEMENT == table_alias_stmt->type) {
		SqlStatement *	    insert_stmt;
		SqlInsertStatement *insert;

		insert_stmt = table_alias_stmt;
		UNPACK_SQL_STATEMENT(insert, insert_stmt, insert);
		assert(NULL == parent_join);
		assert(NULL == parent_table_alias);
		assert(NULL == ret->ret_cla);
		result |= qualify_query(insert->src_table_alias_stmt, NULL, NULL, ret);
		/* Check if insert->columns to src_table_alias_stmt columns mapping is valid. If not issue error.
		 * Deferred till this point as we need the expanded column list in case of * or table.* usage.
		 * TODO: avoid deferring the following processing in cases where its not required. Refer:
		 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/816#note_583771101
		 */
		if (NULL != insert->columns) {
			SqlColumnList *	    start_cl, *cur_cl;
			SqlTableAlias *	    src_table_alias;
			SqlColumnListAlias *start_cla, *cur_cla;
			SqlStatement *	    table_alias_stmt;

			table_alias_stmt = drill_to_table_alias(insert->src_table_alias_stmt);
			UNPACK_SQL_STATEMENT(src_table_alias, table_alias_stmt, table_alias);
			UNPACK_SQL_STATEMENT(start_cla, src_table_alias->column_list, column_list_alias);
			cur_cla = start_cla;
			UNPACK_SQL_STATEMENT(start_cl, insert->columns, column_list);
			cur_cl = start_cl;
			do {
				if (NULL == cur_cla) {
					ERROR(ERR_INSERT_TOO_MANY_COLUMNS, NULL);
					yyerror(NULL, NULL, &cur_cl->value, NULL, NULL, NULL);
					return 1;
				}
				cur_cl = cur_cl->next;
				if (NULL != cur_cla) {
					cur_cla = cur_cla->next;
					if (start_cla == cur_cla) {
						cur_cla = NULL;
					}
				}
			} while (cur_cl != start_cl);
			if (NULL != cur_cla) {
				ERROR(ERR_INSERT_TOO_MANY_EXPRESSIONS, NULL);
				yyerror(NULL, NULL, &cur_cla->column_list, NULL, NULL, NULL);
				return 1;
			}
		}
		/* There is nothing to qualify in "insert->dst_table_alias" and "insert->columns" */
		return result;
	}
	if (set_operation_STATEMENT == table_alias_stmt->type) {
		SqlSetOperation *set_opr;

		UNPACK_SQL_STATEMENT(set_opr, table_alias_stmt, set_operation);
		result |= qualify_query(set_opr->operand[0], parent_join, parent_table_alias, ret);
		result |= qualify_query(set_opr->operand[1], parent_join, parent_table_alias, ret);
		return result;
	}
	assert(table_alias_STATEMENT == table_alias_stmt->type);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
	if (NULL == table_alias->parent_table_alias) {
		table_alias->parent_table_alias = parent_table_alias;
	} else {
		assert(table_alias->parent_table_alias == parent_table_alias);
	}
	table_type = table_alias->table->type;
	switch (table_type) {
	case create_table_STATEMENT:
		return result;
		break;
	case table_value_STATEMENT:
		/* For a table constructed using the VALUES clause, go through each value specified and look for
		 * any sub-queries. If so, qualify those. Literal values can be skipped.
		 */
		UNPACK_SQL_STATEMENT(table_value, table_alias->table, table_value);
		UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
		start_row_value = row_value;
		do {
			result |= qualify_statement(row_value->value_list, parent_join, table_alias_stmt, 0, NULL);
			row_value = row_value->next;
		} while (row_value != start_row_value);
		return result;
		break;
	default:
		assert(select_STATEMENT == table_type);
		break;
	}
	UNPACK_SQL_STATEMENT(select, table_alias->table, select);
	UNPACK_SQL_STATEMENT(join, select->table_list, join);

	/* Ensure strict column name qualification checks (i.e. all column name references have to be a valid column
	 * name in a valid existing table) by using NULL as the last parameter in various `qualify_statement()` calls below.
	 */
	table_alias->do_group_by_checks = FALSE; /* need to set this before invoking "qualify_statement()" */
	start_join = cur_join = join;
	/* Qualify FROM clause first. For this qualification, only use tables from the parent query FROM list.
	 * Do not use any tables from the current query level FROM list for this qualification.
	 */
	do {
		SqlStatement *stmt = join->value;

		/* Qualify sub-queries involved in the join. Note that it is possible a `table` is involved in the join instead
		 * of a `sub-query` in which case the below `qualify_query` call will return right away.
		 */
		result |= qualify_query(cur_join->value, parent_join, table_alias, ret);
		/* The following code block needs to be after above call to qualify_query() because we need
		 * any asterisk usage in cur_join->value to be expanded and qualified for the later natural_join_condition() to work
		 * correctly.
		 * TODO: avoid deferring the following processing in cases where its not required. Refer:
		 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/816#note_583771101
		 */
		if (NATURAL_JOIN == cur_join->type) {
			/* cur_join->condition would not yet have been filled in (deferred in parser). Do that here and
			 * at the same time do some qualification checks too (errors will be returned as a non-zero value).
			 * Its possible to defer this logic till this point because asterisk and table.* are processed in
			 * qualify_query() itself. The asterisk processing logic requires the below call to set duplicates columns
			 * correctly.
			 */
			if (natural_join_condition(start_join, cur_join)) {
				return 1;
			}
		}
		/* Copy correlation specification i.e. aliases. We defer it till this point as asterisk and table.* expansion has to
		 * be complete before we can determine if the correlation specification is valid or not.
		 * TODO: avoid deferring the following processing in cases where its not required. Refer:
		 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/816#note_583771101
		 */
		stmt = drill_to_table_alias(stmt);
		if (table_alias_STATEMENT == stmt->type) {
			SqlTableAlias *join_table_alias;

			UNPACK_SQL_STATEMENT(join_table_alias, stmt, table_alias);
			if (join_table_alias->correlation_specification != NULL) {
				if (copy_correlation_specification_aliases(join_table_alias))
					return 1;
			}
		}
		cur_join = cur_join->next;
	} while (cur_join != start_join);
	/* Now that FROM clause has been qualified, qualify the JOIN conditions etc. in the FROM clause.
	 * Also add in joins (if any) from higher/parent level queries so sub-queries (current level) can use them.
	 * And for this part we can use tables from the FROM list at this level. In some cases we can only use
	 * a partial FROM list (as the loop progresses, the list size increases in some cases). In other cases (NATURAL JOIN)
	 * we can use the full FROM list.
	 */
	prev_start = join;
	prev_end = join->prev;
	if (NULL != parent_join) {
		dqappend(join, parent_join);
	}
	cur_join = join;
	do {
		SqlJoin *next_join;

		/* Make sure any table.column references in the ON condition of the JOIN (cur_join->condition) are qualified
		 * until the current table in the join list (i.e. forward references should not be allowed). Hence the set of
		 * "cur_join->next" below (to "start_join" effectively hiding the remaining tables to the right).
		 * See YDBOcto#291 for example query that demonstrates why this is needed.
		 */
		next_join = cur_join->next; /* save join list before tampering with it */
		/* Note that if "parent_join" is non-NULL, we need to include that even though it comes
		 * after all the tables in the join list at the current level. This is so any references
		 * to columns in parent queries are still considered as valid. Hence the parent_join check below.
		 */
		cur_join->next = ((NULL != parent_join) ? parent_join : start_join); /* stop join list at current join */
		table_alias->aggregate_depth = AGGREGATE_DEPTH_FROM_CLAUSE;
		result |= qualify_statement(cur_join->condition, start_join, table_alias_stmt, 0, NULL);
		cur_join->next = next_join; /* restore join list to original */
		cur_join = next_join;
	} while ((cur_join != start_join) && (cur_join != parent_join));
	// Qualify WHERE clause next
	table_alias->aggregate_depth = AGGREGATE_DEPTH_WHERE_CLAUSE;
	if (NULL != ret) {
		ret->ret_cla = NULL;
		/* Note: Inherit ret.max_unique_id from caller (could be parent/outer query in case this is a sub-query) as is */
	}
	result |= qualify_statement(select->where_expression, start_join, table_alias_stmt, 0, ret);
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
		SqlColumnListAlias *start_cla, *cur_cla;
		SqlTableAlias *	    group_by_table_alias;
		SqlColumnList *	    col_list;
		int		    group_by_column_count;

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
			if (column_alias_STATEMENT == col_list->value->type) {
				SqlColumnAlias *column_alias;

				UNPACK_SQL_STATEMENT(column_alias, col_list->value, column_alias);
				UNPACK_SQL_STATEMENT(group_by_table_alias, column_alias->table_alias_stmt, table_alias);
				if (group_by_table_alias->parent_table_alias != table_alias) {
					/* Column belongs to an outer query. Discard it from the GROUP BY list. */
					SqlColumnListAlias *prev, *next;

					prev = cur_cla->prev;
					next = cur_cla->next;
					prev->next = next;
					next->prev = prev;
					if ((cur_cla == start_cla) && (next != cur_cla)) {
						start_cla = cur_cla = next;
						continue;
					}
				} else {
					if (0 == group_by_column_count) {
						group_by_expression->v.column_list_alias = cur_cla;
					}
					group_by_column_count++;
				}
			} else {
				/* This is a case of an invalid column name specified in the GROUP BY clause.
				 * An "Unknown column" error would have already been issued about this (i.e. result would be 1).
				 * Assert both these conditions.
				 */
				assert(result);
				assert((value_STATEMENT == col_list->value->type)
				       && ((COLUMN_REFERENCE == col_list->value->v.value->type)
					   || (TABLE_ASTERISK == col_list->value->v.value->type)));
			}
			cur_cla = cur_cla->next;
			if (cur_cla == start_cla) {
				/* Note: Cannot move the negation of the above check to the `while(TRUE)` done below
				 * because there is a `continue` code path above which should go through without any checks.
				 */
				break;
			}
		} while (TRUE);
		/* The "|| result" case below is to account for query errors (e.g. "Unknown column" error, see comment above) */
		assert((group_by_column_count == table_alias->group_by_column_count) || result);
		if (!group_by_column_count) {
			select->group_by_expression = NULL;
		}
	}
	ret_cla = NULL;
	table_alias->aggregate_depth = 0;
	for (;;) {
		QualifyStatementParms lcl_ret;

		assert(0 == table_alias->aggregate_depth);
		// Qualify HAVING clause
		result |= qualify_statement(select->having_expression, start_join, table_alias_stmt, 0, NULL);
		// Qualify SELECT column list next
		/* Expand ASTERISK here using prev_start and prev_end
		 * Deferred till this point as we might come across a table.* usage in the join and that might be referring to a
		 * parent query table.
		 * TODO: avoid deferring the following processing in cases where its not required. Refer:
		 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/816#note_583771101
		 */
		if ((!table_alias->do_group_by_checks) && (NULL != select->select_list)
		    && (column_list_alias_STATEMENT == select->select_list->type)) {
			SqlStatement *	    select_list;
			SqlColumnListAlias *cla_cur, *cla_head, *asterisk_list;

			select_list = select->select_list;
			cla_cur = cla_head = select->select_list->v.column_list_alias;
			asterisk_list = NULL;
			/* Go through the select column list (`select n1.id,*,n2.id ...`) to find/process ASTERISK */
			do {
				if (NULL == cla_cur->column_list) {
					/* Came across an ASTERISK in the select column list */
					if (NULL == asterisk_list) {
						SqlJoin *next_join = NULL;

						if (prev_end != start_join->prev) {
							/* Parent join exists. Remove them temporarily so process_asterisk() expands
							 * * to only all current query level columns and not parent query columns */
							next_join = prev_end->next;
							start_join->prev->next = next_join;
							next_join->prev = start_join->prev;
							start_join->prev = prev_end;
							prev_end->next = start_join;
						}
						asterisk_list = process_asterisk(start_join, select_list->loc);
						if (NULL != next_join) {
							/* Add back the parent join nodes */
							prev_end->next = next_join;
							start_join->prev = next_join->prev;
							next_join->prev->next = start_join;
							next_join->prev = prev_end;
						}
					}
					if (cla_cur->next == cla_cur) {
						/* cla_cur is the only member of select column list (`select * from ..`) */
						select_list->v.column_list_alias = asterisk_list;
					} else {
						SqlColumnListAlias *cla_alias;

						cla_alias = copy_column_list_alias_list(asterisk_list, NULL, NULL, NULL);
						/* ASTERISK is present among other columns `select n1.id,*,n2.id from ..`
						 * Replace dummy node (current cla_cur) with column alias list corresponding
						 * to ASTERISK in the position where it was seen in select column list.
						 */
						REPLACE_COLUMNLISTALIAS(cla_cur, cla_alias, cla_head, select_list);
					}
				}
				cla_cur = cla_cur->next;
			} while (cla_cur != cla_head);
		}
		result |= qualify_statement(select->select_list, start_join, table_alias_stmt, 0, NULL);
		// Qualify ORDER BY clause next
		/* Now that all column names used in the query have been qualified, allow columns specified in
		 * ORDER BY to be qualified against any column names specified till now without any strict checking.
		 * Hence the use of a non-NULL value ("&ret_cla") for "lcl_ret->ret_cla".
		 */
		lcl_ret.ret_cla = &ret_cla;
		lcl_ret.max_unique_id = ((NULL != ret) ? ret->max_unique_id : NULL);
		result |= qualify_statement(select->order_by_expression, start_join, table_alias_stmt, 0, &lcl_ret);
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
