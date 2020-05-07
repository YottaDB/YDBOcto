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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

SqlKey *lp_get_key(LogicalPlan *plan, LogicalPlan *lp_column_alias) {
	SqlColumnAlias		*column_alias;
	SqlColumn		*column;
	SqlValue		*key_table_name, *search_table_name, *key_column_name, *search_column_name;
	SqlTableAlias		*table_alias;
	SqlTable		*table;
	SqlColumnListAlias	*cl_alias;
	SqlKey			*key, *primary_key;
	int			key_id, search_id, join_table_id, join_table_num;
	LogicalPlan		*cur_key, *lp_key;
	boolean_t		first_matching_key;

	column_alias = lp_column_alias->v.lp_column_alias.column_alias;
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
	search_id = table_alias->unique_id;
	UNPACK_SQL_STATEMENT(table, table_alias->table, table);
	UNPACK_SQL_STATEMENT(search_table_name, table->tableName, value);

	if (column_alias->column->type == column_STATEMENT) {
		UNPACK_SQL_STATEMENT(column, column_alias->column, column);
		UNPACK_SQL_STATEMENT(search_column_name, column->columnName, value);
	} else {
		UNPACK_SQL_STATEMENT(cl_alias, column_alias->column, column_list_alias);
		UNPACK_SQL_STATEMENT(search_column_name, cl_alias->alias, value);
	}

	cur_key = lp_get_keys(plan);

	primary_key = NULL;
	first_matching_key = TRUE;
	join_table_num = 0;
	join_table_id = -1;
	do {
		GET_LP(lp_key, cur_key, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		key_id = key->unique_id;
		if (join_table_id != key_id)
		{
			join_table_id = key_id;
			join_table_num++;
		}
		do {
			if (key_id != search_id)
				break;
			/* If the table has a composite key and we found some of those columns already
			 * fixed (LP_KEY_FIX) but found one column that is not so then we need to not
			 * consider the primary key for this table as fixed (i.e. all columns corresponding
			 * to the primary key need to be fixed in order to not generate an xref key for
			 * a non-primary-key column). But if this table is not the first in a sequence of tables
			 * that are joined, then even in this case of not all columns being fixed, it is better
			 * (for performance reasons) to let some primary key column stay fixed instead of
			 * generating a cross-reference.
			 */
			if ((NULL != primary_key) && (LP_KEY_FIX != key->type) && (1 >= join_table_num))
				primary_key = NULL;
			if (first_matching_key && (LP_KEY_FIX == key->type))
				primary_key = key;
			first_matching_key = FALSE;
			/// TODO: the only way something has a name of NULL is if it's an output key
			// Which means we're looking for the key in a derived table; we don't currently
			// support this
			if (NULL == key->table) {
				assert(NULL == key->column);
				break;
			}
			UNPACK_SQL_STATEMENT(key_table_name, key->table->tableName, value);
			if (0 != strcmp(search_table_name->v.string_literal, key_table_name->v.string_literal))
				break;
			UNPACK_SQL_STATEMENT(key_column_name, key->column->columnName, value);
			if (0 != strcmp(search_column_name->v.string_literal, key_column_name->v.string_literal))
				break;
			return key;
		} while (TRUE);
		cur_key = cur_key->v.lp_default.operand[1];
	} while (cur_key != NULL);
	if (NULL != primary_key) {
		/* If primary key is already fixed, then no point trying to generate xref key for the
		 * same table. Return non-NULL value (corresponding to the primary key for this table)
		 * so we skip xref generation in caller function "lp_optimize_where_multi_equal_ands_helper()".
		 * In case this table has a composite key, we pick the first of those keys and return it.
		 */
		return primary_key;
	}
	return NULL;
}
