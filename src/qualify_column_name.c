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

#include "logical_plan.h"

/**
 * Tries to find the column in the list of tables. If the name is already qualified, verifies the table exists.
 *
 * For the case of join tables, searches using the <tableName>.<columnName>
 *  followed by searching without separating the two parts
 *
 * This function returns a pointer to a SqlColumnAlias structure if the column name we are searching for is found.
 *	In this case, "*ret_cla" is untouched.
 * But if the column name is found only in an alias name, it returns the corresponding SqlColumnListalias pointer in
 *	"*ret_cla" and the function return value is NULL.
 * Note that it is possible we return NULL as the function return value and "*ret_cla" is also set to NULL. In this case
 *	though, an ERR_UNKNOWN_COLUMN_NAME error would have been issued in this function and caller knows to handle that.
 */
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt, int depth,
				    SqlColumnListAlias **ret_cla) {
	SqlColumnListAlias *start_cla, *cur_cla, *col_cla, *t_col_cla, *group_by_alias_cla;
	SqlJoin *	    cur_join, *start_join;
	SqlStatement *	    matching_alias_stmt;
	SqlValue *	    value;
	char *		    table_name, *column_name, *c, *first_delim;
	int		    table_name_len, column_name_len;
	boolean_t	    group_by_alias_ambiguous;

	matching_alias_stmt = NULL;
	// If the value is not a column_reference or TABLE_ASTERISK, we should not be here
	assert((COLUMN_REFERENCE == column_value->type) || (TABLE_ASTERISK == column_value->type));

	// Find the first period; if it is missing, we need to match against
	//  all columns in all tables
	for (c = column_value->v.string_literal; ('\0' != *c) && ('.' != *c); c++) {
		// Pass
	}
	if ('.' == *c) {
		first_delim = c;
		/* Until issue YDBOcto#139 is resolved, i.e. databases are supported, there are two possibilities here:
		 *	1. There is a table name and a column name, e.g. table_name.column_name
		 *	2. There is a database name before the table name and column name, e.g. database_name.table_name.column_name
		 * Accordingly, here we check to see if there is a database name specified by continuing past the initial '.'
		 * delimiter to see if another '.' is present. If so, that means a database name is included and, until
		 * YDBOcto#139 is resolved, should be treated as part of the table name.
		 */
		for (c++; ('\0' != *c) && ('.' != *c); c++) {
			// Pass
		}
		table_name = column_value->v.reference;
		if ('.' == *c) {
			table_name_len = c - table_name;
			column_name = c + 1;
			column_name_len = strlen(column_name);
		} else {
			table_name_len = first_delim - table_name;
			column_name = first_delim + 1;
			column_name_len = strlen(column_name);
		}
	} else {
		column_name = column_value->v.reference;
		column_name_len = c - column_name;
		table_name = NULL;
		table_name_len = 0;
	}
	col_cla = NULL;
	assert(NULL != table_alias_stmt);
	group_by_alias_cla = NULL;
	group_by_alias_ambiguous = FALSE;
	/* Check if ret_cla is non-NULL. If so, this means we are matching a column reference in the ORDER BY or GROUP BY clause.
	 * In this case, we should first check if any alias name specified explicitly by the user in the SELECT column list, or a
	 * column reference name in the SELECT column list matches the column name. If so match that. If not, we then later try to
	 * match this column name against an input column name in the JOIN list of tables. Also check if the column reference in the
	 * ORDER BY or GROUP BY clause has a table name specified. If it does, then we cannot match it against the SELECT column
	 * list so skip this "if" block.
	 */
	if ((NULL != ret_cla) && (NULL == table_name)) {
		SqlTableAlias *table_alias;
		SqlColumnList *column_list;

		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
		cur_cla = start_cla;
		do {
			UNPACK_SQL_STATEMENT(column_list, cur_cla->column_list, column_list);
			if ((NULL != cur_cla->alias)
			    && (cur_cla->user_specified_alias || (column_alias_STATEMENT == column_list->value->type))) {
				DEBUG_ONLY(assert(column_list == column_list->next));
				DEBUG_ONLY(assert(column_list == column_list->prev));
				UNPACK_SQL_STATEMENT(value, cur_cla->alias, value);
				if (((int)strlen(value->v.reference) == column_name_len)
				    && (0 == memcmp(value->v.reference, column_name, column_name_len))) {
					if (NULL != col_cla) {
						/* GROUP BY and ORDER BY ambiguity resolution is handled differently. Hence the
						 * below `if` check.
						 * In case of GROUP BY column alias usage:
						 * 1) If an input column exists with the same name as select column alias, choose
						 *    the former.
						 * 2) If an input column doesn't exist and there is ambiguity between select column
						 *    aliases, issue error. Refer to
						 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1140#note_1003010368 for
						 * more details.
						 */
						if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth) {
							// We want to issue this error later if no input column matches the alias
							group_by_alias_ambiguous = TRUE;
							break;
						} else {
							ERROR(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
							return NULL;
						}
					}
					col_cla = cur_cla;
					matching_alias_stmt = table_alias_stmt;
				}
			}
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);
		if (NULL != col_cla) {
			/* We qualified an input column name as a valid name because it matched the alias name of an
			 * already qualified column in the SELECT column list. But in this case, we only allow alias
			 * names to be used as is. Not inside an expression. Check for that.
			 * "depth" (the nesting of "qualify_statement" calls) is guaranteed to be at least 3 here.
			 * Following is the C-stack in case alias name is used as is.
			 *	depth=0 : "qualify_statement() : case column_list_alias_STATEMENT"
			 *	depth=1 : "qualify_statement() : case column_list_STATEMENT"
			 *	depth=2 : "qualify_statement() : case column_list_STATEMENT"
			 *	depth=3 : "qualify_column_name()"
			 * If alias name is used in an expression though, depth would be GREATER THAN 3. So check for that.
			 */
			assert(3 <= depth);
			if (3 >= depth) {
				if (AGGREGATE_DEPTH_GROUP_BY_CLAUSE == table_alias->aggregate_depth) {
					/* Continue on to the rest of the code which checks for an input column matching
					 * the column name we are looking for. If one is found use that instead of the
					 * found select list column alias corresponding cla as we want to solve such
					 * ambiguity by choosing the input column over select list column in case of GROUP
					 * BY.
					 */
					group_by_alias_cla = col_cla;
					col_cla = NULL;
				} else {
					/* Set the matched SqlColumnListAlias structure pointer in "*ret_cla". But function return
					 * value will stay NULL. Caller knows to check "*ret_cla" in this case.
					 */
					*ret_cla = col_cla;
					return NULL;
				}
			} else {
				/* Treat the match as a no-match since the alias name ended up inside an expression.
				 * Fall through to code that will issue ERR_UNKNOWN_COLUMN_NAME error below.
				 */
				col_cla = NULL;
			}
		}
	}
	/* If "col_cla" is non-NULL at this point, this means the column reference is in an ORDER BY clause and it was matched
	 * by a user-specified column alias in the SELECT column list. If so, use that as the match and move on. No need to
	 * go into the "if" block below in that case.
	 */
	SqlStatement *set_oper_stmt = NULL;
	if (NULL == col_cla) {
		if (NULL != tables) {
			cur_join = start_join = tables;
			do {
				SqlStatement *sql_stmt;
				boolean_t     ambiguous;

				sql_stmt = drill_to_table_alias(cur_join->value);
				// If we need to match a table, ensure this table is the correct one before calling the helper
				SqlTableAlias *cur_table_alias;
				UNPACK_SQL_STATEMENT(cur_table_alias, sql_stmt, table_alias);
				if (NULL != table_name) {
					if (NULL != cur_table_alias->alias) {
						int table_name_len2;

						UNPACK_SQL_STATEMENT(value, cur_table_alias->alias, value);
						table_name_len2 = strlen(value->v.reference);
						if ((table_name_len == table_name_len2)
						    && (0 == memcmp(value->v.reference, table_name, table_name_len))) {
							matching_alias_stmt = sql_stmt;
							if (TABLE_ASTERISK == column_value->type) {
								SqlColumnAlias *ret;

								/* This is a table.asterisk value and exists only as an indication
								 * to later stages to consider the entire row value. Return a
								 * "SqlColumnAlias" with "TABLE_ASTERISK" value and matching
								 * "table_alias_stmt". Note: We need to return the same column alias
								 * in case "n1.*" is specified multiple times (for the "n1" table)
								 * in the same query (or else GROUP BY validation related to
								 * "group_by_column_number" field in the column alias cannot work)
								 */
								if (NULL == cur_table_alias->table_asterisk_column_alias) {
									OCTO_CMALLOC_STRUCT(ret, SqlColumnAlias);
									SQL_VALUE_STATEMENT(ret->column, TABLE_ASTERISK,
											    column_value->v.string_literal);
									ret->table_alias_stmt = matching_alias_stmt;
									SQL_STATEMENT(cur_table_alias->table_asterisk_column_alias,
										      column_alias_STATEMENT);
									cur_table_alias->table_asterisk_column_alias->v.column_alias
									    = ret;
								} else {
									ret = cur_table_alias->table_asterisk_column_alias->v
										  .column_alias;
								}
								if (set_operation_STATEMENT == cur_join->value->type) {
									ret->set_oper_stmt = cur_join->value;
								}
								return ret;
							}
							col_cla = match_column_in_table(cur_table_alias, column_name,
											column_name_len, &ambiguous, TRUE);
							if ((NULL != col_cla) && ambiguous) {
								/* There are multiple column matches within one table in the
								 * FROM list. An error has already been issued inside
								 * "match_column_in_table". Record error context here.
								 * Signal an error return from this function by returning
								 * NULL.
								 */
								yyerror(NULL, NULL, &cur_table_alias->alias, NULL, NULL, NULL);
								return NULL;
							}
							if (set_operation_STATEMENT == cur_join->value->type) {
								set_oper_stmt = cur_join->value;
							}
							break;
						}
					}
				} else {
					t_col_cla = match_column_in_table(cur_table_alias, column_name, column_name_len, &ambiguous,
									  TRUE);
					if (NULL != t_col_cla) {
						if (ambiguous) {
							/* There are multiple column matches within one table in the FROM list. An
							 * error has already been issued inside "match_column_in_table". Record
							 * error context here. Signal an error return from this function by
							 * returning NULL.
							 */
							yyerror(NULL, NULL, &sql_stmt, NULL, NULL, NULL);
							return NULL;
						}
						if (NULL != col_cla) {
							/* There are multiple column matches. Check if the matches came across
							 * multiple tables in the FROM list at the same level or a different level.
							 * Issue an error in the former case. Do not issue an error in the latter
							 * case (consider closest level table as matching).
							 */
							SqlTableAlias *matching_table_alias;

							UNPACK_SQL_STATEMENT(matching_table_alias, matching_alias_stmt,
									     table_alias);
							/* Note that "cur_table_alias->parent_table_alias" would be non-NULL at
							 * this point in case of a SELECT query because the entire query output
							 * itself is an on-the-fly constructed TABLE (and corresponds to the
							 * "parent_table_alias"). But in case of a DELETE FROM or UPDATE query
							 * that has a sub-query in the WHERE clause, it is possible that
							 * "matching_table_alias" points to a table in the sub-query while
							 * "cur_table_alias" points to the table name being deleted/updated in
							 * the DELETE FROM/UPDATE query. "cur_table_alias->parent_table_alias"
							 * would be NULL in that case. Therefore, we cannot assert that
							 * "cur_table_alias->parent_table_alias" is non-NULL just like
							 * we are able to do with "matching_table_alias->parent_table_alias" below.
							 */
							assert(NULL != matching_table_alias->parent_table_alias);
							assert(NULL != cur_table_alias->parent_table_alias);
							if (cur_table_alias->parent_table_alias->v.table_alias
							    == matching_table_alias->parent_table_alias->v.table_alias) {
								ERROR(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
								return NULL;
							} else {
								/* In the JOIN list, we have gone past all tables in the first
								 * matching table level. No need to even scan for any more tables in
								 * the join list.
								 */
								break;
							}
						}
						matching_alias_stmt = sql_stmt;
						col_cla = t_col_cla;
						if (set_operation_STATEMENT == cur_join->value->type) {
							set_oper_stmt = cur_join->value;
						}
					}
				}
				cur_join = cur_join->next;
			} while (cur_join != start_join);
		}
		if (NULL == col_cla) {
			if (NULL != group_by_alias_cla) {
				/* This is a GROUP BY alias usage and the alias is matched with select list column.
				 * Check if any ambiguity existed.
				 */
				if (group_by_alias_ambiguous) {
					ERROR(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
					return NULL;
				} else {
					/* Return NULL and set `ret_cla` so that GROUP BY column list alias is updated with the node
					 * found. By waiting this till point in case of ambiguity, GROUP BY name will be interpreted
					 * as an input-column name rather than an output column name.
					 */
					*ret_cla = group_by_alias_cla;
					return NULL;
				}
			} /* else no cla was found issue appropriate error by going through the below code */
			if (NULL == table_name) {
				ERROR(ERR_UNKNOWN_COLUMN_NAME, column_name);
			} else if (NULL != matching_alias_stmt) {
				/* A table name was specified along with the column name and we found the table name
				 * matched with an existing table in the FROM/JOIN list. This means the column name is
				 * not found in the specified table name. Issue same error as above but with table name
				 * included in the column name. "table_name" points to a string of the form
				 * "tablename.columnname" hence using that below to print both table and column name.
				 */
				ERROR(ERR_UNKNOWN_COLUMN_NAME, table_name);
			} else {
				/* table_name is non-NULL and "matching_alias_stmt" is NULL. This means a table name was
				 * explicitly specified but we did not find that in any of the FROM/JOIN list. Issue a
				 * table related error (different than the above column related errors). "table_name"
				 * points to a string of the form "tablename.columnname" but we want to only print the
				 * tablename hence the use of "table_name_len" below to stop there.
				 */
				ERROR(ERR_MISSING_FROM_ENTRY, table_name_len, table_name);
			}
			return NULL;
		}
	}
	SqlColumnAlias *column_alias = get_column_alias_for_column_list_alias(col_cla, matching_alias_stmt);
	if (NULL != set_oper_stmt) {
		column_alias->set_oper_stmt = set_oper_stmt;
	}
	return column_alias;
}
