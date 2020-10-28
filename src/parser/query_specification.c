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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include <string.h>

/* In case where ASTERISK or TABLENAME.ASTERISK is the first element
 * in `column_list_alias` list (select *,n1.id ..), re-initialize the head.
 * Update `cla_cur` to point to the end of newly merged asterisk list. This ensures
 * that the column value next to current ASTERISK is processed in next iteration.
 */
#define REPLACE_COLUMNLISTALIAS(CLA_CUR, CLA_ALIAS, CLA_HEAD, LIST) \
	{                                                           \
		SqlColumnListAlias *rm_cla = (CLA_CUR);             \
                                                                    \
		(CLA_CUR) = (CLA_CUR)->next;                        \
		dqdel(rm_cla);                                      \
		dqappend(CLA_CUR, CLA_ALIAS);                       \
		if ((CLA_HEAD) == rm_cla) {                         \
			(LIST)->v.column_list_alias = CLA_ALIAS;    \
			(CLA_HEAD) = (CLA_ALIAS);                   \
		}                                                   \
		(CLA_CUR) = (CLA_CUR)->prev;                        \
	}

/* If the select list is empty (i.e. "*" was specified), we need all columns from the joins in the order in which they
 * are mentioned. There is an exception and that is if NATURAL JOIN is specified in the query. If so, the "*" will
 * be expanded to the following list of columns (in this order):
 *	1) All the common columns in the order they are specified in the left table.
 *	2) Every remaining non-common column from the left table in the order it appears in that table.
 *	3) Every remaining non-common column from the right table in the order it appears in that table.
 * asterisk_table_name represents TABLENAME.* value present in select column list. It is set to NULL in all other case.
 */
SqlColumnListAlias *process_asterisk(SqlSelectStatement *select, char *asterisk_table_name, struct YYLTYPE loc) {
	SqlStatement *	    sql_stmt;
	SqlColumnListAlias *cla_common, *t_cla_alias, *cla_alias;
	SqlJoin *	    join, *cur_join, *start_join;
	SqlTableAlias *	    table_alias;
	int		    tablejoin_num, comp_result, asterisk_table_name_len;

	UNPACK_SQL_STATEMENT(join, select->table_list, join);
	start_join = cur_join = join;
	tablejoin_num = 1;
	comp_result = 0;
	cla_alias = NULL;
	asterisk_table_name_len = ((NULL == asterisk_table_name) ? 0 : strlen(asterisk_table_name) - 2);
	do {
		sql_stmt = cur_join->value;
		sql_stmt = drill_to_table_alias(sql_stmt);
		UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
		if (NULL != table_alias->column_list) {
			SqlColumnListAlias *cla_start, *cla_cur;
			SqlColumnAlias *    column_alias;
			boolean_t	    common_column_seen;

			if (NULL != asterisk_table_name) {
				/* Case where TABLENAME.ASTERISK is being processed */
				SqlValue *alias_value;

				UNPACK_SQL_STATEMENT(alias_value, table_alias->alias, value);
				comp_result = strncmp(alias_value->v.string_literal, asterisk_table_name, asterisk_table_name_len);
				if (0 != comp_result) {
					/* cur_join is not what we are looking for, move to next join */
					cur_join = cur_join->next;
					tablejoin_num++;
					continue;
				}
			}
			UNPACK_SQL_STATEMENT(cla_start, table_alias->column_list, column_list_alias);
			common_column_seen = FALSE;
			cla_cur = cla_start;
			do {
				SqlColumnListAlias *cla_primary;
				SqlColumnListAlias *cla_new;

				cla_primary = ((NULL == asterisk_table_name) ? cla_cur->duplicate_of_column : NULL);
				if (NULL == cla_primary) {
					SqlColumnList *cur;

					OCTO_CMALLOC_STRUCT(cla_new, SqlColumnListAlias);
					cla_new->alias = cla_cur->alias;
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
					 * The overload not required when processing TABLENAME.ASTERISK
					 */
					if (NULL == asterisk_table_name)
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
		}
		if (NULL != asterisk_table_name) {
			if (NULL == table_alias->column_list) {
				assert(cur_join == cur_join->next);
				/* ERROR Case `select n1.*;` - no from clause */
				ERROR(ERR_MISSING_FROM_ENTRY, asterisk_table_name_len, asterisk_table_name);
				return NULL;
			}
			/* cla for TABLENAME.ASTERISK is added, skip further processing. */
			break;
		} else {
			/* Here ASTERISK usage is being processed continue to process all joins */
			cur_join = cur_join->next;
			tablejoin_num++;
		}
	} while (cur_join != start_join);
	if ((NULL != asterisk_table_name) && (0 != comp_result)) {
		/* This is only reached when TABLENAME does not correspond to any of the joins
		 * ERROR Case `select n2.* from names n1;`
		 */
		ERROR(ERR_MISSING_FROM_ENTRY, asterisk_table_name_len, asterisk_table_name);
		return NULL;
	}
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

/* Function invoked by the rule named "query_specification" in src/parser/select.y
 * Responsible to process ASTERISK and TABLENAME.ASTERISK usage in select column list and
 * to process TABLENAME.ASTERISK usage in order by clause.
 */
SqlStatement *query_specification(OptionalKeyword set_quantifier, SqlStatement *select_list, SqlStatement *table_expression,
				  SqlStatement *sort_specification_list, int *plan_id) {
	SqlStatement *	    ret, *quantifier;
	SqlTableAlias *	    this_table_alias;
	SqlSelectStatement *select;
	SqlColumnListAlias *asterisk_list;
	SqlColumnListAlias *cla_cur, *cla_head;
	char *		    order_by_name;
	int		    order_by_name_len;

	SQL_STATEMENT(ret, table_alias_STATEMENT);
	MALLOC_STATEMENT(ret, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(this_table_alias, ret, table_alias);
	SQL_VALUE_STATEMENT(this_table_alias->alias, NUL_VALUE, "");
	assert(select_STATEMENT == table_expression->type);
	this_table_alias->table = table_expression;
	this_table_alias->unique_id = (*plan_id)++;
	assert(column_list_alias_STATEMENT == select_list->type);
	this_table_alias->column_list = select_list;
	UNPACK_SQL_STATEMENT(select, table_expression, select);
	select->select_list = select_list;
	cla_cur = cla_head = select_list->v.column_list_alias;
	asterisk_list = NULL;
	/* Go through the select column list (`select n1.id,*,n2.id ...`) to find/process ASTERISK */
	do {
		if (NULL == cla_cur->column_list) {
			/* Came across an ASTERISK in the select column list */
			if (NULL == asterisk_list)
				asterisk_list = process_asterisk(select, NULL, select_list->loc);
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
		} else {
			SqlColumnList *inner_column_list;

			UNPACK_SQL_STATEMENT(inner_column_list, cla_cur->column_list, column_list);
			if (value_STATEMENT == inner_column_list->value->type) {
				char *	  column_name;
				int	  column_name_len;
				SqlValue *value;

				UNPACK_SQL_STATEMENT(value, inner_column_list->value, value);
				column_name = value->v.reference;
				column_name_len = strlen(value->v.reference);
				if ((column_name[column_name_len - 2] == '.') && (column_name[column_name_len - 1] == '*')) {
					/* Came across an TABLENAME.ASTERISK in the select column list */
					SqlColumnListAlias *t_asterisk_list;

					t_asterisk_list = process_asterisk(select, column_name, select_list->loc);
					if (NULL == t_asterisk_list) {
						yyerror(NULL, NULL, &(cla_cur->column_list), NULL, NULL, NULL);
						return NULL;
					}
					if (cla_cur->next == cla_cur) {
						/* cla_cur is the only member of select column list (`select * from ..`) */
						select_list->v.column_list_alias = t_asterisk_list;
					} else {
						SqlColumnListAlias *cla_alias;

						cla_alias = t_asterisk_list;
						/* TABLE.ASTERISK is present among other columns `select n1.id,n1.*,n2.id from ..`
						 * Replace dummy node (current cla_cur) with column alias list corresponding to
						 * TABLE.ASTERISK in the position where it was seen in select column list.
						 */
						REPLACE_COLUMNLISTALIAS(cla_cur, cla_alias, cla_head, select_list);
					}
				}
			}
		}
		cla_cur = cla_cur->next;
	} while (cla_cur != cla_head);
	SQL_STATEMENT(quantifier, keyword_STATEMENT);
	OCTO_CMALLOC_STRUCT(quantifier->v.keyword, SqlOptionalKeyword);
	quantifier->v.keyword->keyword = set_quantifier;
	quantifier->v.keyword->v = NULL;
	dqinit(quantifier->v.keyword);
	select->optional_words = quantifier;
	if (NULL == sort_specification_list) {
		/* No ORDER BY */
		select->order_by_expression = sort_specification_list;
		return ret;
	}
	/* Replace TABLENAME.ASTERISK in sort_specification_list with column list corresponding to TABLENAME table.
	 * Error is thrown when FROM entry for TABLENAME is missing for the query.
	 */
	assert(column_list_alias_STATEMENT == sort_specification_list->type);
	cla_head = cla_cur = sort_specification_list->v.column_list_alias;
	do {
		SqlColumnList *inner_column_list;

		UNPACK_SQL_STATEMENT(inner_column_list, cla_cur->column_list, column_list);
		if (value_STATEMENT == inner_column_list->value->type) {
			SqlValue *value;

			UNPACK_SQL_STATEMENT(value, inner_column_list->value, value);
			order_by_name = value->v.reference;
			order_by_name_len = strlen(order_by_name);
			if ((order_by_name[order_by_name_len - 2] == '.') && (order_by_name[order_by_name_len - 1] == '*')) {
				/* Came across TABLENAME.ASTERISK, iterate through the join and find the table */
				SqlJoin *	    join, *cur_join, *start_join;
				SqlStatement *	    sql_stmt;
				SqlTableAlias *	    table_alias;
				SqlColumnListAlias *result;
				SqlValue *	    alias_value;
				int		    comp_res;

				UNPACK_SQL_STATEMENT(join, select->table_list, join);
				start_join = cur_join = join;
				result = NULL;
				do {
					sql_stmt = cur_join->value;
					sql_stmt = drill_to_table_alias(sql_stmt);
					UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
					UNPACK_SQL_STATEMENT(alias_value, table_alias->alias, value);
					comp_res = strncmp(alias_value->v.string_literal, order_by_name, order_by_name_len - 2);
					if ((0 == comp_res) && (NULL != table_alias->column_list)) {
						SqlColumnListAlias *cla;

						UNPACK_SQL_STATEMENT(cla, table_alias->column_list, column_list_alias);
						result = copy_column_list_alias_list(cla, sql_stmt, cla_cur->keywords);
						REPLACE_COLUMNLISTALIAS(cla_cur, result, cla_head, sort_specification_list);
						break;
					}
					cur_join = cur_join->next;
				} while (cur_join != start_join);
				if ((cur_join == start_join) && (NULL == result)) {
					/* ERROR no join match the order_by_name */
					ERROR(ERR_MISSING_FROM_ENTRY, order_by_name_len - 2, order_by_name);
					yyerror(NULL, NULL, &(cla_cur->column_list), NULL, NULL, NULL);
					return NULL;
				}
			}
		}
		cla_cur = cla_cur->next;
	} while (cla_head != cla_cur);
	select->order_by_expression = sort_specification_list;
	return ret;
}
