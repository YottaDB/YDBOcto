/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

#define SET_PARENT_TABLE_ALIAS_TO_SELF(TABLE_ALIAS)                                                                       \
	{                                                                                                                 \
		/* The table alias for a column alias corresponding to a SELECT query has a non-NULL parent table alias   \
		 * (an extra table alias is created in "src/parser/query_specification.c"). But for INSERT, UPDATE and    \
		 * DELETE commands, the parent table alias is NULL. But this poses problems when we invoke the            \
		 * CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR macro as that relies on "parent_table_alias" being non-NULL \
		 * in various places for aggregate function related processing (issuing ERR_AGGREGATE_FUNCTION_WHERE or   \
		 * ERR_AGGREGATE_FUNCTION_UPDATE errors etc.). Therefore, we set the parent table alias to be itself.     \
		 * This does not pose any problems else where and so is safe to do so.                                    \
		 */                                                                                                       \
		assert(NULL != TABLE_ALIAS->parent_table_alias);                                                          \
		SQL_STATEMENT(TABLE_ALIAS->parent_table_alias, table_alias_STATEMENT);                                    \
		TABLE_ALIAS->parent_table_alias->v.table_alias = TABLE_ALIAS;                                             \
	}

/* Set qualify stage variables (e.g. WHERE clause OR UPDATE SET clause etc.) before the CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR
 * call in the caller of this macro so errors like ERR_AGGREGATE_FUNCTION_WHERE or ERR_AGGREGATE_FUNCTION_UPDATE etc. in the
 * caller command are correctly issued.
 */
#define SET_QUALIFY_STAGE(TBL_STMT, TBL_ALIAS, AGGR_DEPTH_STAGE, QUALIFY_QUERY_STAGE) \
	{                                                                             \
		UNPACK_SQL_STATEMENT(TBL_ALIAS, TBL_STMT, table_alias);               \
		assert(0 == TBL_ALIAS->aggregate_depth);                              \
		TBL_ALIAS->aggregate_depth = AGGR_DEPTH_STAGE;                        \
		assert(QualifyQuery_NONE == TBL_ALIAS->qualify_query_stage);          \
		TBL_ALIAS->qualify_query_stage = QUALIFY_QUERY_STAGE;                 \
	}

#define RESET_QUALIFY_STAGE(TBL_ALIAS, AGGR_DEPTH_STAGE, QUALIFY_QUERY_STAGE)  \
	{                                                                      \
		assert(AGGR_DEPTH_STAGE == TBL_ALIAS->aggregate_depth);        \
		TBL_ALIAS->aggregate_depth = 0;                                \
		assert(QUALIFY_QUERY_STAGE == TBL_ALIAS->qualify_query_stage); \
		TBL_ALIAS->qualify_query_stage = QualifyQuery_NONE;            \
	}

/* Returns:
 *	0 if query is successfully qualified.
 *	1 if query had errors during qualification.
 */
int qualify_query(SqlStatement *table_alias_stmt, SqlJoin *parent_join, SqlTableAlias *parent_table_alias,
		  QualifyStatementParms *ret) {
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

	// Below are variables used in multiple "case" blocks below so are declared before the "switch" statement.
	SqlJoin *join;

	switch (table_alias_stmt->type) {
	case insert_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlStatement *	    insert_stmt;
		SqlInsertStatement *insert;

		insert_stmt = table_alias_stmt;
		UNPACK_SQL_STATEMENT(insert, insert_stmt, insert);
		assert(NULL == parent_join);
		assert(NULL == parent_table_alias);
		assert(NULL == ret->ret_cla);
		CALL_QUALIFY_QUERY_AND_RETURN_ON_ERROR(insert->src_table_alias_stmt, NULL, NULL, ret, result);
		/* Some error checks happened already in "insert_statement.c". Do some more here.
		 * If "insert->columns" is non-NULL, check if "insert->columns" and "src_table_alias_stmt" has same number of
		 * columns and issue error otherwise.
		 * If "insert->columns" is NULL, similar errors will be issued later in "check_column_lists_for_type_match()"
		 * (called by "populate_data_type()").
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
		/* There is nothing to qualify in "insert->dst_table_alias_stmt" and nothing more to qualify in "insert->columns".
		 * Qualification of column names in "insert->columns" happened already as part of the "find_column()" call in
		 * "src/parser/insert_statement.c".
		 */
		return result;
		break;
	case delete_from_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlStatement *delete_stmt;
		SqlDeleteFromStatement *delete;

		delete_stmt = table_alias_stmt;
		UNPACK_SQL_STATEMENT(delete, delete_stmt, delete_from);
		assert(NULL == parent_join);
		assert(NULL == parent_table_alias);
		assert(NULL == ret->ret_cla);
		UNPACK_SQL_STATEMENT(join, delete->src_join, join);
		assert(join == join->next);
		assert(NULL == join->condition);
		CALL_QUALIFY_QUERY_AND_RETURN_ON_ERROR(join->value, parent_join, parent_table_alias, ret, result);
		SET_QUALIFY_STAGE(join->value, table_alias, AGGREGATE_DEPTH_WHERE_CLAUSE, QualifyQuery_WHERE);
		SET_PARENT_TABLE_ALIAS_TO_SELF(table_alias);
		CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(delete->where_clause, join, join->value, 0, ret, result);
		RESET_QUALIFY_STAGE(table_alias, AGGREGATE_DEPTH_WHERE_CLAUSE, QualifyQuery_WHERE);
		/* Now that call is done, reset qualify stage variables to what they were before */
		return result;
		break;
	case update_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlStatement *	    update_stmt;
		SqlUpdateStatement *update;

		update_stmt = table_alias_stmt;
		UNPACK_SQL_STATEMENT(update, update_stmt, update);
		assert(NULL == parent_join);
		assert(NULL == parent_table_alias);
		assert(NULL == ret->ret_cla);
		UNPACK_SQL_STATEMENT(join, update->src_join, join);
		assert(join == join->next);
		assert(NULL == join->condition);
		CALL_QUALIFY_QUERY_AND_RETURN_ON_ERROR(join->value, parent_join, parent_table_alias, ret, result);
		SET_QUALIFY_STAGE(join->value, table_alias, AGGREGATE_DEPTH_WHERE_CLAUSE, QualifyQuery_WHERE);
		SET_PARENT_TABLE_ALIAS_TO_SELF(table_alias);
		CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(update->where_clause, join, join->value, 0, ret, result);
		RESET_QUALIFY_STAGE(table_alias, AGGREGATE_DEPTH_WHERE_CLAUSE, QualifyQuery_WHERE);

		SqlUpdateColumnValue *ucv, *ucv_head;
		ucv_head = update->col_value_list;
		ucv = ucv_head;
		SET_QUALIFY_STAGE(join->value, table_alias, AGGREGATE_DEPTH_UPDATE_SET_CLAUSE, QualifyQuery_UPDATE_SET_CLAUSE);
		do {
			/* Qualifying "ucv->col_name" happened already as part of the "find_column()"
			 * call in "src/parser/update_statement.c". So skip qualifying that here.
			 */
			CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(ucv->col_value, join, join->value, 0, ret, result);
			ucv = ucv->next;
		} while (ucv != ucv_head);
		RESET_QUALIFY_STAGE(table_alias, AGGREGATE_DEPTH_UPDATE_SET_CLAUSE, QualifyQuery_UPDATE_SET_CLAUSE);
		return result;
		break;
	case set_operation_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlSetOperation *set_opr;

		UNPACK_SQL_STATEMENT(set_opr, table_alias_stmt, set_operation);
		CALL_QUALIFY_QUERY_AND_RETURN_ON_ERROR(set_opr->operand[0], parent_join, parent_table_alias, ret, result);
		CALL_QUALIFY_QUERY_AND_RETURN_ON_ERROR(set_opr->operand[1], parent_join, parent_table_alias, ret, result);
		return result;
		break;
	default:
		break;
	}
	assert(table_alias_STATEMENT == table_alias_stmt->type);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
	if (NULL == table_alias->parent_table_alias) {
		SQL_STATEMENT(table_alias->parent_table_alias, table_alias_STATEMENT);
		table_alias->parent_table_alias->v.table_alias = parent_table_alias;
	} else {
		assert(NULL != table_alias->parent_table_alias);
		assert(table_alias->parent_table_alias->v.table_alias == parent_table_alias);
	}
	table_type = table_alias->table->type;
	switch (table_type) {
	case create_table_STATEMENT:
		return result;
		break;
	case create_view_STATEMENT:
		// The create_view_STATEMENT which has a SELECT statement is qualified and stored so nothing to qualify again
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
			CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(row_value->value_list, parent_join, table_alias_stmt, 0, ret,
								   result);
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
	/* Qualify list of tables in FROM and JOIN clauses first. For this qualification, only use tables from the parent query
	 * FROM/JOIN list. Do not use any tables from the current query level FROM/JOIN list for this qualification.
	 */
	do {
		SqlStatement *stmt = cur_join->value;

		/* Qualify sub-queries involved in the join. Note that it is possible a `table` is involved in the join instead
		 * of a `sub-query` in which case the below `qualify_query` call will return right away.
		 */
		CALL_QUALIFY_QUERY_AND_RETURN_ON_ERROR(stmt, parent_join, table_alias, ret, result);
		/* The following code block needs to be after above call to qualify_query() because we need any asterisk usage
		 * in "stmt" to be expanded and qualified for the later natural_join_condition() to work correctly.
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
		/* Copy correlation specification i.e. aliases. We defer it till this point as "*" and "TABLE.*" expansion
		 * has to be complete before we can determine if the correlation specification is valid or not.
		 */
		SqlTableAlias *join_table_alias;
		stmt = drill_to_table_alias(stmt);
		assert(table_alias_STATEMENT == stmt->type);
		UNPACK_SQL_STATEMENT(join_table_alias, stmt, table_alias);
		if (NULL != join_table_alias->correlation_specification) {
			if (copy_correlation_specification_aliases(join_table_alias))
				return 1;
		}
		cur_join = cur_join->next;
	} while (cur_join != start_join);
	/* Now that table names in FROM/JOIN clause have been qualified, qualify the JOIN conditions.
	 * Also add in joins (if any) from higher/parent level queries so sub-queries (current level) can use them.
	 * And for this part we can use table names from the FROM/JOIN list at this level. In some cases we can only use
	 * a partial FROM list (as the loop progresses, the list size increases in some cases). In other cases (NATURAL JOIN)
	 * we can use the full list of table names in the FROM/JOIN list.
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
		CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(cur_join->condition, start_join, table_alias_stmt, 0, ret, result);
		cur_join->next = next_join; /* restore join list to original */
		cur_join = next_join;
	} while ((cur_join != start_join) && (cur_join != parent_join));
	// Qualify WHERE clause next
	table_alias->aggregate_depth = AGGREGATE_DEPTH_WHERE_CLAUSE;
	table_alias->qualify_query_stage = QualifyQuery_WHERE;
	if (NULL != ret) {
		ret->ret_cla = NULL;
		/* Note: Inherit ret->max_unique_id from caller (could be parent/outer query in case this is a sub-query) as is */
	}
	CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(select->where_expression, start_join, table_alias_stmt, 0, ret, result);
	table_alias->aggregate_depth = 0;
	/* Expand "*" usage in SELECT column list here. This was not done in "query_specification.c" when the "*" usage was
	 * first encountered because the FROM/JOIN list of that query could in turn contain a "TABLENAME.*" usage that refers
	 * to a parent query table. In that case "query_specification.c" does not have access to the parent query context.
	 * Therefore it cannot do "*" expansion then. We deferred this processing till here as all parent query contexts are
	 * available only here. See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/816#note_583797217 for details.
	 */
	SqlStatement *	    select_list;
	SqlColumnListAlias *cla_cur, *cla_head, *asterisk_list;
	select_list = select->select_list;
	assert((NULL != select_list) && (column_list_alias_STATEMENT == select_list->type));
	cla_cur = cla_head = select_list->v.column_list_alias;
	asterisk_list = NULL;
	/* Go through the select column list (`select n1.id,*,n2.id ...`) to find/process ASTERISK */
	do {
		SqlColumnList *column_list;
		SqlValue *     value;
		UNPACK_SQL_STATEMENT(column_list, cla_cur->column_list, column_list);
		if (value_STATEMENT == column_list->value->type) {
			UNPACK_SQL_STATEMENT(value, column_list->value, value);
			if (SELECT_ASTERISK == value->type) {
				/* Came across an ASTERISK in the select column list */
				if (NULL == value->v.string_literal) {
					/* This is a case of ERR_SELECT_STAR_NO_TABLES error which should have been
					 * issued under the "query_specification" rule in "src/parser/select.y" but
					 * was delayed to avoid misleading error messages. Now that the possibility of
					 * a misleading error message is gone (any syntax errors would have been issued
					 * already) issue the deferred error here.
					 */
					ERROR(ERR_SELECT_STAR_NO_TABLES, NULL);
					yyerror(NULL, NULL, &cla_cur->column_list, NULL, NULL, NULL);
					return 1;
				}
				if (NULL == asterisk_list) {
					SqlJoin *next_join = NULL;

					if (prev_end != start_join->prev) {
						/* Parent join exists. Remove them temporarily so process_asterisk() expands
						 * to only all current query level columns and not parent query columns
						 */
						next_join = prev_end->next;
						start_join->prev->next = next_join;
						next_join->prev = start_join->prev;
						start_join->prev = prev_end;
						prev_end->next = start_join;
					}
					asterisk_list = process_asterisk(start_join, cla_cur->column_list->loc);
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

					cla_alias = copy_column_list_alias_list(asterisk_list, NULL, NULL);
					/* ASTERISK is present among other columns `select n1.id,*,n2.id from ..`
					 * Replace dummy node (current cla_cur) with column alias list corresponding
					 * to ASTERISK in the position where it was seen in select column list.
					 */
					REPLACE_COLUMNLISTALIAS(cla_cur, cla_alias, cla_head, select_list);
				}
			}
		}
		cla_cur = cla_cur->next;
	} while (cla_cur != cla_head);

	SqlColumnListAlias *  ret_cla;
	QualifyStatementParms lcl_ret, *lcl_ret_ptr;
	ret_cla = NULL;
	/* Initialize "lcl_ret->ret_cla" to a non-NULL value (&ret_cla) and pass "&lcl_ret" (through the "lcl_ret_ptr" variable)
	 * in case of qualifying ORDER BY and GROUP BY. This allows columns specified in these clauses to be qualified against
	 * any column names specified till now (including aliases specified after an AS) without any strict checking.
	 */
	lcl_ret.ret_cla = &ret_cla;
	lcl_ret.max_unique_id = ((NULL != ret) ? ret->max_unique_id : NULL);
	lcl_ret.aggr_table_alias_stmt = ((NULL != ret) ? ret->aggr_table_alias_stmt : NULL);
	lcl_ret_ptr = &lcl_ret;

	int *save_max_unique_id = NULL;
	/* We qualify SELECT, HAVING and ORDER BY clauses TWICE below. Hence the "for" loop.
	 * This is because
	 * 1) In case of SELECT, HAVING and ORDER BY we want to check if any aggregate functions were used anywhere even if a
	 *    GROUP BY was not used. In that case, we want to ensure there are no non-grouped column references in the remainder of
	 *    the query. See comment where "do_group_by_checks" member is defined in the "SqlTableAlias" structure in
	 *    "octo_types.h".
	 * 2) With implementation of GROUP BY column numbers and expressions
	 *    * GROUP BY qualification will happen after qualification of SELECT in the first iteration of the `for` loop
	 *      This is because column number references can only be qualified after SELECT qualification is complete
	 *    * Because GROUP BY qualification is completed only after first iteration we need another iteration to perform
	 *      expression matching in SELECT, HAVING and ORDER BY
	 * 3) Additionally, in case of HAVING the following two conditions are neccessary to go through qualify twice.
	 *    * Expression matching happens root first
	 *    * In this case if we try to match HAVING clause columns in the first iteration itself, we will be
	 *      comparing a GROUP BY column which is formed out of fully qualified `stmt` and a column formed out of
	 *      unqualified HAVING clause stmt. This is incorrect.
	 *    * To avoid the above scenario we qualify HAVING in first iteration and then make use of the second
	 *      to perform expression matching.
	 */
	for (;;) {
		// First iteration:
		//	Qualify each clause
		//	Qualify each element
		//	Replace column number usages in GROUP BY and ORDER BY
		//	Assign GROUP BY column numbers to column references
		//	Issue GROUP BY subquery usage error if any
		//	Issue SELECT DISTINCT related error in ORDER BY
		//	Expand table.* in SELECT
		//	Expand table.* in ORDER BY if `aggregate_function_or_group_by_or_having_specified` is 0 at the end
		//		of first iteration.
		// Second iteration:
		//	Assign GROUP BY column numbers to expressions
		//	Issue error on GROUP BY validation failure
		assert(0 == table_alias->aggregate_depth);
		// Qualify SELECT column list first
		table_alias->qualify_query_stage = QualifyQuery_SELECT_COLUMN_LIST;
		CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(select->select_list, start_join, table_alias_stmt, 0, ret, result);
		// Qualify GROUP BY clause next
		/* 1. Deferred till this point as we want select list to be qualified before GroupBy as we want to refer to the
		 *    qualified select list through GroupBy column numbers.
		 * 2. GROUP BY qualification is only done once even though it is inside the `for` loop. Error reporting and
		 *    column number replacement for GROUP BY is taken care in the first iteration itelf.
		 */
		group_by_expression = select->group_by_expression;
		table_alias->qualify_query_stage = QualifyQuery_NONE;
		if (!table_alias->do_group_by_checks) {
			/* Note that while `GROUP_BY_SPECIFIED` bit of
			 * `table_alias->aggregate_function_or_group_by_or_having_specified` will mostly be 0 at this point, it
			 * is possible for it to be 1 in some cases (see YDBOcto#457 for example query) if this `qualify_query()`
			 * invocation corresponds to a sub-query in say the HAVING clause of an outer query. In that case,
			 * `qualify_query()` for the sub-query would be invoked twice by the `qualify_query()` of the outer query
			 * because of the current `for` loop. If so, GROUP BY expression processing for the sub-query happens
			 * for the second time. This second time processing will perform GROUP BY validations on the GROUP BY
			 * clause. This is necessary as we need to ensure ungrouped outer query column references are identified
			 * and an error is issued for such usages.
			 */
			if (NULL != group_by_expression) {
				int group_by_column_count;

				table_alias->aggregate_depth = AGGREGATE_DEPTH_GROUP_BY_CLAUSE;
				CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(group_by_expression, start_join, table_alias_stmt, 0,
									   lcl_ret_ptr, result);
				ret_cla = NULL; // Re-set the variable so that ORDER BY can re-use it
				/* After GROUP BY qualification `table_alias->group_by_column_count` can still be 0 if GROUP BY was
				 * done on a parent query column. Even in this case, since GROUP BY was specified, we need to do
				 * GROUP BY related checks. Setting GROUP_BY_SPECIFIED value to the below field ensures the checks
				 * happen.
				 */
				table_alias->aggregate_function_or_group_by_or_having_specified |= GROUP_BY_SPECIFIED;

				/* Traverse the GROUP BY list to see what columns belong to this table_alias. Include only those in
				 * the GROUP BY list. Exclude any other columns (e.g. columns belonging to outer query) from the
				 * list as they are constant as far as this sub-query is concerned.
				 *
				 * Do not clear `GROUP_BY_SPECIFIED` bit in `aggregate_function_or_group_by_or_having_specified`
				 * even if all columns are excluded as we want to perform GROUP BY validations even if the column
				 * list is empty.
				 */
				SqlColumnListAlias *start_cla, *cur_cla;
				SqlColumnList *	    col_list;

				UNPACK_SQL_STATEMENT(start_cla, group_by_expression, column_list_alias);
				group_by_column_count = 0;
				cur_cla = start_cla;
				do {
					UNPACK_SQL_STATEMENT(col_list, cur_cla->column_list, column_list);
					if (column_alias_STATEMENT == col_list->value->type) {
						SqlColumnAlias *column_alias;
						SqlTableAlias * group_by_table_alias;

						UNPACK_SQL_STATEMENT(column_alias, col_list->value, column_alias);
						UNPACK_SQL_STATEMENT(group_by_table_alias, column_alias->table_alias_stmt,
								     table_alias);
						if (0 == group_by_column_count) {
							group_by_expression->v.column_list_alias = cur_cla;
						}
						assert(NULL != group_by_table_alias->parent_table_alias);
						if (group_by_table_alias->parent_table_alias->v.table_alias != table_alias) {
							/* Column belongs to an outer query. Do not increment
							 * current query's `group_by_column_count`.
							 */
						} else {
							group_by_column_count++;
						}
					} else {
						/* If an error existed we would have
						 * not reached this code as we return immediately after qualify_statement() call
						 * when `result` is `1`.
						 */
						assert(!result);
						/* We have come across an expression usage.
						 * Note: we do not remove an expression even when it refers to columns from outer
						 * query, this is to avoid parameter numbering difference which occurs if constants
						 * are part of the expression and they are removed. If such a case is allowed the
						 * constants which were parameterized later in this expression will refer to the
						 * wrong parameter number.
						 */
						if (0 == group_by_column_count) {
							group_by_expression->v.column_list_alias = cur_cla;
						}
						group_by_column_count++;
					}
					cur_cla = cur_cla->next;
					if (cur_cla == start_cla) {
						/* Note: Cannot move the negation of the above check to the `while(TRUE)` done below
						 * because there is a `continue` code path above which should go through without any
						 * checks.
						 */
						break;
					}
				} while (TRUE);
				assert(group_by_column_count == table_alias->group_by_column_count);
			}
		}
		if (NULL != select->having_expression) {
			table_alias->qualify_query_stage = QualifyQuery_NONE;
			table_alias->aggregate_depth = AGGREGATE_DEPTH_HAVING_CLAUSE;
			CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(select->having_expression, start_join, table_alias_stmt, 0, ret,
								   result);
			table_alias->aggregate_function_or_group_by_or_having_specified |= HAVING_SPECIFIED;
		}
		// Qualify ORDER BY clause next (see comment above for why "lcl_ret_ptr" is passed for ORDER BY).
		table_alias->qualify_query_stage = QualifyQuery_ORDER_BY;
		table_alias->aggregate_depth = 0;
		CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(select->order_by_expression, start_join, table_alias_stmt, 0,
							   lcl_ret_ptr, result);
		/* Expansion of table.* in ORDER BY is done here so that expansion can be skipped when GROUP BY/HAVING/Aggregate
		 * exists. At this point we can safely rely on `aggregate_function_or_group_by_or_having_specified` to indicate if
		 * the entire query has any form of grouping or not.
		 * More details : https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1109#note_961299737.
		 */
		SqlOptionalKeyword *keywords, *keyword;
		UNPACK_SQL_STATEMENT(keywords, select->optional_words, keyword);
		keyword = get_keyword_from_keywords(keywords, OPTIONAL_DISTINCT);
		if ((0 == result) && !table_alias->do_group_by_checks && (NULL != select->order_by_expression)) {
			SqlColumnListAlias *start_cla, *cur_cla;
			UNPACK_SQL_STATEMENT(start_cla, select->order_by_expression, column_list_alias);
			cur_cla = start_cla;
			do {
				SqlColumnList *cur_cl;
				UNPACK_SQL_STATEMENT(cur_cl, cur_cla->column_list, column_list);
				if (column_alias_STATEMENT == cur_cl->value->type) {
					SqlColumnAlias *column_alias;
					UNPACK_SQL_STATEMENT(column_alias, cur_cl->value, column_alias);

					SqlTableAlias *cur_table_alias;
					UNPACK_SQL_STATEMENT(cur_table_alias, column_alias->table_alias_stmt, table_alias);
					assert(NULL != cur_table_alias->parent_table_alias);
					if ((0
					     == cur_table_alias->parent_table_alias->v.table_alias
						    ->aggregate_function_or_group_by_or_having_specified)
					    && is_stmt_table_asterisk(column_alias->column)) {
						// Expand table.* in ORDER BY as there is no grouping done in the query
						process_table_asterisk_cla(select->order_by_expression, &cur_cla, &start_cla,
									   table_alias->qualify_query_stage);
					}
				}
				/* Check if SELECT DISTINCT was specified */
				if (NULL != keyword) {
					/* SELECT DISTINCT was specified. Check if the ORDER BY column expression
					 * matches some column specification in the SELECT column list. If so that
					 * is good. If not issue an error (see YDBOcto#461 for details).
					 */
					if (!match_column_list_alias_in_select_column_list(cur_cla, select->select_list)) {
						ERROR(ERR_ORDER_BY_SELECT_DISTINCT, "");
						yyerror(NULL, NULL, &cur_cla->column_list, NULL, NULL, NULL);
						return 1;
					}
				}
				cur_cla = cur_cla->next;
			} while (cur_cla != start_cla);
		}
		table_alias->qualify_query_stage = QualifyQuery_NONE;
		if (!table_alias->aggregate_function_or_group_by_or_having_specified) {
			/* GROUP BY or AGGREGATE function or HAVING was never used in the query.
			 * No need to do GROUP BY validation checks.
			 */
			break;
		} else if (table_alias->do_group_by_checks) {
			/* GROUP BY or AGGREGATE function or HAVING was used in the query. And GROUP BY validation checks
			 * already done as part of the second iteration in this for loop. Can now break out of the loop.
			 */
			break;
		}
		/* GROUP BY or AGGREGATE function or HAVING was used in the query. Assign GROUP BY number and validate */
		table_alias->do_group_by_checks = TRUE;
		/* Disable "max_unique_id" computation (using "stmt->hash_canonical_query_cycle") in "qualify_statement()"
		 * in the 2nd iteration of this for loop as that takes some shortcuts using the
		 * SET_GROUP_BY_EXPRESSION_COLUMN_NUMBER_AND_BREAK macro in "qualify_statement.c" and that could result
		 * in incorrect values of "*ret->max_unique_id" which would then break the "move_where_clause_to_on_clause()"
		 * optimization and cause incorrect move of WHERE clause conditions to the ON clause (YDBOcto#850). This is achieved
		 * by setting `ret->max_unique_id` and `lcl_ret.max_unique_id` to NULL. Note that `ret` and `lcl_ret` itself is not
		 * set to NULL because these values are used for aggregate processing (YDBOcto#755).
		 */
		if ((NULL != ret) && (NULL != ret->max_unique_id)) {
			save_max_unique_id = ret->max_unique_id;
			ret->max_unique_id = NULL;
			lcl_ret.max_unique_id = NULL;
		}
	}
	table_alias->do_group_by_checks = FALSE;
	if (NULL != save_max_unique_id) {
		assert(NULL != ret);
		ret->max_unique_id = save_max_unique_id;
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
