/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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
 *  followed by searching without seperating the two parts
 */
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt,
					boolean_t match_qualified_columns)
{
	SqlColumnAlias		*ret;
	SqlColumnListAlias	*start_cla, *cur_cla;
	SqlJoin			*cur_join, *start_join;
	SqlStatement		*column, *t_column;
	SqlTableAlias		*cur_alias, *matching_alias, *table_alias;
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
	column = NULL;
	do {
		SqlStatement *sql_stmt;

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
					matching_alias = cur_alias;
					column = match_column_in_table(cur_alias, column_name, column_name_len);
					break;
				}
			}
		} else {
			t_column = match_column_in_table(cur_alias, column_name, column_name_len);
			if (NULL != t_column) {
				if (NULL != column) {
					WARNING(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
					cur_join = cur_join->next;
					continue;
				}
				matching_alias = cur_alias;
				column = t_column;
			}

		}
		cur_join = cur_join->next;
	} while(cur_join != start_join);
	if ((NULL != table_alias_stmt) && (NULL == table_name) && (NULL == column)) {
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
		cur_cla = start_cla;
		do {
			if (NULL != cur_cla->alias) {
				/* Check if corresponding owning column has already been qualified/verified.
				 * If so, we can use this alias for qualification/verification purposes.
				 */
				SqlColumnList	*column_list;
				UNPACK_SQL_STATEMENT(column_list, cur_cla->column_list, column_list);
				assert(column_list == column_list->next);
				assert(column_list == column_list->prev);
				/* If match_qualified_columns is TRUE, we will return an input column name as a valid name
				 *    only if it matches the name of an existing column in the table whose column name has already
				 *    been qualified by a prior call to the "qualify_column_name" function (this is checked by
				 *    the "column_alias_STATEMENT == column_list->value->type" check below.
				 * If match_qualified_columns is FALSE, we will return an input column name as a valid name
				 *    as long as it matches the name of an existing column in the table.
				 */
				if (!match_qualified_columns || (column_alias_STATEMENT == column_list->value->type))
				{
					UNPACK_SQL_STATEMENT(value, cur_cla->alias, value);
					if ((column_name_len == (int)strlen(value->v.reference))
							&& (0 == memcmp(value->v.reference, column_name, column_name_len))) {
						if (NULL != column) {
							WARNING(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
							cur_cla = cur_cla->next;
							continue;
						}
						UNPACK_SQL_STATEMENT(matching_alias, table_alias_stmt, table_alias);
						SQL_STATEMENT(t_column, column_list_alias_STATEMENT);
						t_column->v.column_list_alias = cur_cla;
						column = t_column;
					}
				}
			}
			cur_cla = cur_cla->next;
		} while(cur_cla != start_cla);
	}
	if (NULL == column) {
		// Note: If table_name is non-NULL, it points to a string of the form "tablename.columnname"
		//       so both table name and column name will be printed below if "table_name" is non-NULL.
		ERROR(ERR_UNKNOWN_COLUMN_NAME, (NULL != table_name) ? table_name : column_name);
		return NULL;
	}
	OCTO_CMALLOC_STRUCT(ret, SqlColumnAlias);
	ret->column = column;
	PACK_SQL_STATEMENT(ret->table_alias, matching_alias, table_alias);
	return ret;
}
