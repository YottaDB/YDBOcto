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

SqlStatement *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len);

/**
 * Tries to find the column in the list of tables
 * If the name is already qualifies, verifies the table exists.
 *
 * For the case of join tables, searches using the <tableName>.<columnName>
 *  followed by searching without seperating the two parts
 */
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt) {
	int table_name_len = 0, column_name_len = 0;
	char *table_name = NULL, *column_name = NULL, *c;
	SqlColumnAlias *ret;
	SqlStatement *column = NULL, *t_column;
	SqlTableAlias *cur_alias, *matching_alias, *table_alias;
	SqlColumnListAlias *start_cla, *cur_cla;
	SqlJoin *cur_join, *start_join;
	SqlValue *value;


	// If the value is not a column_reference, we should not be here
	assert(column_value->type == COLUMN_REFERENCE);

	// Find the first period; if it is missing, we need to match against
	//  all columns in all tables
	for(c = column_value->v.string_literal; *c != '\0' && *c != '.'; c++) {
		// Pass
	}
	if(*c == '.') {
		table_name = column_value->v.reference;
		table_name_len = c - table_name;
		column_name = c+1;
		column_name_len = strlen(column_name);
	} else {
		column_name = column_value->v.reference;
		column_name_len = c - column_name;
	}


	cur_join = start_join = tables;
	do {
		// If we need to match a table, ensure this table
		//  is the correct one before calling the helper
		UNPACK_SQL_STATEMENT(cur_alias, cur_join->value, table_alias);
		if(table_name) {
			if(cur_alias->alias != NULL) {
				UNPACK_SQL_STATEMENT(value, cur_alias->alias, value);
				if(memcmp(value->v.reference, table_name, table_name_len) == 0) {
					matching_alias = cur_alias;
					column = match_column_in_table(cur_alias, column_name, column_name_len);
					break;
				}
			}
		} else {
			t_column = match_column_in_table(cur_alias, column_name, column_name_len);
			if(t_column != NULL) {
				if(column != NULL) {
					WARNING(CUSTOM_ERROR, "Ambgious column name");
					return NULL;
				}
				matching_alias = cur_alias;
				column = t_column;
			}

		}
		cur_join = cur_join->next;
	} while(cur_join != start_join);

	if(table_alias_stmt != NULL && table_name == NULL && column == NULL) {
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
		cur_cla = start_cla;
		do {
			if(cur_cla->alias != NULL) {
				UNPACK_SQL_STATEMENT(value, cur_cla->alias, value);
				if(memcmp(value->v.reference, column_name, column_name_len) == 0) {
					if(column != NULL) {
						WARNING(CUSTOM_ERROR, "Ambgious column name");
						return NULL;
					}
					UNPACK_SQL_STATEMENT(matching_alias, table_alias_stmt, table_alias);
					SQL_STATEMENT(t_column, column_list_alias_STATEMENT);
					t_column->v.column_list_alias = cur_cla;
					column = t_column;
				}
			}
			cur_cla = cur_cla->next;
		} while(cur_cla != start_cla);
	}

	if(column == NULL) {
		WARNING(ERR_UNKNOWN_COLUMN_NAME, column_name);
		return NULL;
	}

	ret = (SqlColumnAlias*)octo_cmalloc(memory_chunks, sizeof(SqlColumnAlias));
	ret->column = column;
	PACK_SQL_STATEMENT(ret->table_alias, matching_alias, table_alias);

	return ret;
}
