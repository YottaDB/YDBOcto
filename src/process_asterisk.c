/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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
#include <string.h>

/* If the select list is empty (i.e. "*" was specified), we need all columns from the joins in the order in which they
 * are mentioned. There is an exception and that is if NATURAL JOIN is specified in the query. If so, the "*" will
 * be expanded to the following list of columns (in this order):
 *	1) All the common columns in the order they are specified in the left table.
 *	2) Every remaining non-common column from the left table in the order it appears in that table.
 *	3) Every remaining non-common column from the right table in the order it appears in that table.
 */
SqlColumnListAlias *process_asterisk(SqlJoin *select_table_list, struct YYLTYPE loc) {
	SqlStatement *	    sql_stmt;
	SqlColumnListAlias *cla_common, *t_cla_alias, *cla_alias;
	SqlJoin *	    cur_join, *start_join;
	SqlTableAlias *	    table_alias;
	int		    tablejoin_num;

	cur_join = start_join = select_table_list;
	tablejoin_num = 1;
	cla_alias = NULL;
	do {
		sql_stmt = cur_join->value;
		sql_stmt = drill_to_table_alias(sql_stmt);
		UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);

		SqlColumnListAlias *cla_start, *cla_cur;
		SqlColumnAlias *    column_alias;
		boolean_t	    common_column_seen;
		assert(NULL != table_alias->column_list);
		UNPACK_SQL_STATEMENT(cla_start, table_alias->column_list, column_list_alias);
		common_column_seen = FALSE;
		cla_cur = cla_start;
		do {
			SqlColumnListAlias *cla_primary;
			SqlColumnListAlias *cla_new;

			cla_primary = cla_cur->duplicate_of_column;
			if (NULL == cla_primary) {
				SqlColumnList *cur;

				OCTO_CMALLOC_STRUCT(cla_new, SqlColumnListAlias);
				/* `cla_cur` corresponds to a column in the JOIN list of the query. `cla_new` corresponds to the
				 * column that replaces the `*` usage. Any errors using the replaced cla should point to the
				 * original location of the `*`. Therefore create a copy of the cla and set the location in the
				 * alias of the copy to point to `loc`.
				 */
				cla_new->alias = copy_sql_statement(cla_cur->alias);
				cla_new->alias->loc = loc;
				cla_new->type = cla_cur->type;
				OCTO_CMALLOC_STRUCT(cur, SqlColumnList);
				dqinit(cur);
				column_alias = get_column_alias_for_column_list_alias(cla_cur, sql_stmt);
				PACK_SQL_STATEMENT(cur->value, column_alias, column_alias);
				PACK_SQL_STATEMENT(cla_new->column_list, cur, column_list);
				dqinit(cla_new);
				if (NULL == cla_alias)
					cla_alias = cla_new;
				else
					dqappend(cla_alias, cla_new);
				/* Maintain pointer from table column list alias to select column list alias.
				 * We overload/abuse the "duplicate_of_column" field for this purpose.
				 * This is used by the "else" block below.
				 */
				cla_cur->duplicate_of_column = cla_new;
			} else {
				/* This is a common column. The column that this is a duplicate of has to be
				 * moved ahead in the SELECT column list. We will for now note this column
				 * down and do the move at the end of processing all columns in this right
				 * side table (since common columns have to be in the order they are seen
				 * in the left side table and not the right side table).
				 */
				assert(NATURAL_JOIN == cur_join->type);
				common_column_seen = TRUE;
				/* First go from the cla in the tablejoin to the cla in the select column list */
				cla_primary = cla_primary->duplicate_of_column;
				/* Note down that this cla in the select column list is a common column
				 * on the left side for this right side table. We use the
				 * "duplicate_of_column" field for this purpose by setting it to a unique
				 * number that corresponds to this particular right side table (hence the use
				 * of "tablejoin_num" which is incremented for every table in the join list).
				 */
				cla_primary->duplicate_of_column = (void *)(intptr_t)tablejoin_num;
				assert(NULL != cla_alias);
			}
			cla_cur = cla_cur->next;
		} while (cla_start != cla_cur);
		if (common_column_seen) {
			SqlColumnListAlias *cla_end;

			cla_common = NULL;
			/* Now that at least one common column has been seen, rearrange all columns
			 * of the left side so the common columns go first.
			 */
			assert(NULL != cla_alias);
			cla_end = cla_alias->prev;
			cla_cur = cla_alias;
			do {
				SqlColumnListAlias *cla_next;

				cla_next = cla_cur->next;
				if ((void *)(intptr_t)tablejoin_num == cla_cur->duplicate_of_column) {
					cla_cur->duplicate_of_column = NULL;
					/* This column is a common column. Move it to a separate common list. */
					/* Before the "dqdel" is done (which would modify "cla_primary->next")
					 * do any "cla_alias" (non-common list of columns) related adjustments.
					 */
					if (cla_cur == cla_alias) {
						/* Reset "cla_alias" now that "cla_cur" is no longer going to be
						 * part of the non-common list of columns.
						 */
						cla_alias = cla_cur->next;
						if (cla_alias == cla_cur) {
							/* There was only 1 column and that turned out to be a
							 * common column. Reset the non-common list to be NULL.
							 */
							cla_alias = NULL;
						}
					}
					dqdel(cla_cur); /* Remove "cla_cur" from its current position */
					/* Now move "cla_cur" to the tail of the "cla_common" doubly linked list */
					if (NULL == cla_common) {
						cla_common = cla_cur;
					} else {
						dqappend(cla_common, cla_cur);
					}
				}
				if (cla_cur == cla_end) {
					break;
				}
				cla_cur = cla_next;
			} while (TRUE);
			assert(NULL != cla_common);
			if (NULL != cla_alias) {
				dqappend(cla_common, cla_alias);
				cla_alias = cla_common;
			} else {
				cla_alias = cla_common;
			}
		}
		/* Here ASTERISK usage is being processed continue to process all joins */
		cur_join = cur_join->next;
		tablejoin_num++;
	} while (cur_join != start_join);
	/* Copy location of ASTERISK (noted down in ASTERISK rule in "src/parser/select.y")
	 * for potential error reporting (with line/column context) in "populate_data_type.c".
	 * Do this in all the created column list aliases.
	 */
	t_cla_alias = cla_alias;
	if (NULL != t_cla_alias) {
		do {
			t_cla_alias->column_list->loc = loc;
			t_cla_alias = t_cla_alias->next;
		} while (t_cla_alias != cla_alias);
	}
	return cla_alias;
}
