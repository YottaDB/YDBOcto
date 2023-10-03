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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

SqlKey *lp_get_key(LogicalPlan *plan, LogicalPlan *lp_column_alias) {
	SqlColumnAlias *column_alias;
	SqlColumn *	column;
	SqlValue *	search_column_name;
	SqlTableAlias * table_alias;
	SqlKey *	key, *primary_key;
	int		key_id, search_id, join_table_id;
	LogicalPlan *	cur_key, *lp_key;

	column_alias = lp_column_alias->v.lp_column_alias.column_alias;
	UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias_stmt, table_alias);
	if (create_table_STATEMENT != table_alias->table->type) {
		/* There is no key for an on-the-fly table constructued using the VALUES clause */
		assert(table_value_STATEMENT == table_alias->table->type);
		return NULL;
	}
	assert(column_alias->column->type == column_STATEMENT);
	UNPACK_SQL_STATEMENT(column, column_alias->column, column);
	UNPACK_SQL_STATEMENT(search_column_name, column->columnName, value);

	cur_key = lp_get_keys(plan);

	primary_key = NULL;
	join_table_id = -1;
	search_id = table_alias->unique_id;
	do {
		GET_LP(lp_key, cur_key, 0, LP_KEY);
		key = lp_key->v.lp_key.key;
		key_id = key->unique_id;
		if (join_table_id != key_id) {
			join_table_id = key_id;
		}
		do {
			if (key_id != search_id)
				break;
			/* If the table has a composite key and we find at least one of those key columns already
			 * fixed (LP_KEY_FIX) we need to consider the keys for this table as fixed even though
			 * there might be other key columns in the composite key that are not fixed. Hence set
			 * "primary_key" to a non-NULL value in that case which would ensure we return a non-NULL
			 * value from this function at the end (which would cause the caller to not generate an xref
			 * plan for this boolean condition). Not doing so could cause multiple key columns to be
			 * fixed in multiple calls to this function resulting in incorrect removal of boolean
			 * conditions from the WHERE clause ending up in incorrect query results (YDBOcto#877).
			 */
			if (LP_KEY_FIX == key->type)
				primary_key = key;
			/// TODO: the only way something has a name of NULL is if it's an output key
			// Which means we're looking for the key in a derived table; we don't currently
			// support this
			if (NULL == key->table) {
				assert(NULL == key->column);
				break;
			}
#ifndef NDEBUG
			SqlTable *table;
			SqlValue *key_table_name, *search_table_name;

			UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
			UNPACK_SQL_STATEMENT(search_table_name, table->tableName, value);
			UNPACK_SQL_STATEMENT(key_table_name, key->table->tableName, value);
			/* We are guaranteed the below assert because "key_id" is same as "search_id" */
			assert(0 == strcmp(search_table_name->v.string_literal, key_table_name->v.string_literal));
#endif

			SqlValue *key_column_name;

			UNPACK_SQL_STATEMENT(key_column_name, key->column->columnName, value);
			if (0 != strcmp(search_column_name->v.string_literal, key_column_name->v.string_literal))
				break;
			return key;
		} while (TRUE);
		GET_LP_ALLOW_NULL(cur_key, cur_key, 1, LP_KEYS);
	} while (NULL != cur_key);
	if (NULL != primary_key) {
		/* If primary key is already fixed, then no point trying to generate xref key for the
		 * same table. Return non-NULL value (corresponding to the primary key for this table)
		 * so we skip xref generation in caller function "lp_optimize_where_multi_equals_ands_helper()".
		 * In case this table has a composite key, we pick the first of those keys and return it.
		 */
		return primary_key;
	}
	return NULL;
}
