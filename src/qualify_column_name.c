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

#include "logical_plan.h"

/**
 * Tries to find the column in the list of tables
 * If the name is already qualified, verifies the table exists.
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
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt,
									int depth, SqlColumnListAlias **ret_cla)
{
	SqlColumnListAlias	*start_cla, *cur_cla, *col_cla, *t_col_cla;
	SqlJoin			*cur_join, *start_join;
	SqlTableAlias		*cur_alias;
	SqlStatement		*matching_alias_stmt;
	SqlValue		*value;
	char			*table_name, *column_name, *c;
	int			table_name_len, column_name_len;

	// If the value is not a column_reference, we should not be here
	assert(COLUMN_REFERENCE == column_value->type);

	// Find the first period; if it is missing, we need to match against
	//  all columns in all tables
	for (c = column_value->v.string_literal; ('\0' != *c) && ('.' != *c); c++) {
		// Pass
	}
	if ('.' == *c) {
		table_name = column_value->v.reference;
		table_name_len = c - table_name;
		column_name = c+1;
		column_name_len = strlen(column_name);
	} else {
		column_name = column_value->v.reference;
		column_name_len = c - column_name;
		table_name = NULL;
		table_name_len = 0;
	}
	cur_join = start_join = tables;
	col_cla = NULL;
	do {
		SqlStatement	*sql_stmt;
		boolean_t	ambiguous;

		sql_stmt = drill_to_table_alias(cur_join->value);
		// If we need to match a table, ensure this table is the correct one before calling the helper
		UNPACK_SQL_STATEMENT(cur_alias, sql_stmt, table_alias);
		if (NULL != table_name) {
			if (NULL != cur_alias->alias) {
				int table_name_len2;

				UNPACK_SQL_STATEMENT(value, cur_alias->alias, value);
				table_name_len2 = strlen(value->v.reference);
				if ((table_name_len == table_name_len2)
						&& (0 == memcmp(value->v.reference, table_name, table_name_len))) {
					matching_alias_stmt = sql_stmt;
					col_cla = match_column_in_table(cur_alias, column_name, column_name_len, &ambiguous);
					if ((NULL != col_cla) && ambiguous) {
						/* There are multiple column matches within one table in the FROM list.
						 * An error has already been issued inside "match_column_in_table".
						 * Record error context here.
						 * Signal an error return from this function by returning NULL.
						 */
						yyerror(NULL, NULL, &cur_alias->alias, NULL, NULL, NULL);
						return NULL;
					}
					break;
				}
			}
		} else {
			t_col_cla = match_column_in_table(cur_alias, column_name, column_name_len, &ambiguous);
			if (NULL != t_col_cla) {
				if (ambiguous) {
					/* There are multiple column matches within one table in the FROM list.
					 * An error has already been issued inside "match_column_in_table".
					 * Record error context here.
					 * Signal an error return from this function by returning NULL.
					 */
					yyerror(NULL, NULL, &sql_stmt, NULL, NULL, NULL);
					return NULL;
				}
				if (NULL != col_cla) {
					/* There are multiple column matches across multiple tables in the FROM list.
					 * Allow this by choosing the first table for the column reference but signal WARNING.
					 */
					WARNING(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
					cur_join = cur_join->next;
					continue;
				}
				matching_alias_stmt = sql_stmt;
				col_cla = t_col_cla;
			}

		}
		cur_join = cur_join->next;
	} while (cur_join != start_join);
	if ((NULL != table_alias_stmt) && (NULL == table_name) && (NULL == col_cla) && (NULL != ret_cla)) {
		/* ret_cla is non-NULL. This means we are allowed to match the input column name as a valid name
		 *    as long as it matches the name of an existing alias name in the table select column list. Check that.
		 */
		SqlTableAlias	*table_alias;

		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
		cur_cla = start_cla;
		do {
			if (NULL != cur_cla->alias) {
				SqlColumnList	*column_list;

				UNPACK_SQL_STATEMENT(column_list, cur_cla->column_list, column_list);
				assert(column_list == column_list->next);
				assert(column_list == column_list->prev);
				UNPACK_SQL_STATEMENT(value, cur_cla->alias, value);
				if (((int)strlen(value->v.reference) == column_name_len)
						&& (0 == memcmp(value->v.reference, column_name, column_name_len))) {
					if (NULL != col_cla) {
						WARNING(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
						cur_cla = cur_cla->next;
						continue;
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
				/* Set the matched SqlColumnListAlias structure pointer in "*ret_cla". But function return
				 * value will stay NULL. Caller knows to check "*ret_cla" in this case.
				 */
				*ret_cla = col_cla;
				return NULL;
			} else {
				/* Treat the match as a no-match since the alias name ended up inside an expression.
				 * Fall through to code that will issue ERR_UNKNOWN_COLUMN_NAME error below.
				 */
				col_cla = NULL;
			}
		}
	}
	if (NULL == col_cla) {
		// Note: If table_name is non-NULL, it points to a string of the form "tablename.columnname"
		//       so both table name and column name will be printed below if "table_name" is non-NULL.
		ERROR(ERR_UNKNOWN_COLUMN_NAME, (NULL != table_name) ? table_name : column_name);
		return NULL;
	}
	return get_column_alias_for_column_list_alias(col_cla, matching_alias_stmt);
}
