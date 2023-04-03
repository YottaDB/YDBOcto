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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Encapsulates the given cla or column with `SqlColumnListAlias->SqlColumnList->SqlColumnAlias` */
SqlColumnListAlias *get_encapsulated_cla(SqlColumn *cur_column, SqlColumnListAlias *cur_cla, SqlStatement *table_alias_stmt) {
	assert((NULL == cur_column) || (NULL == cur_cla));
	SqlColumnList *	    cur;
	SqlColumnListAlias *column_list_alias;
	SqlColumnAlias *    column_alias;
	SqlStatement *	    stmt;
	SQL_STATEMENT(stmt, column_alias_STATEMENT);
	MALLOC_STATEMENT(stmt, column_alias, SqlColumnAlias);
	column_alias = stmt->v.column_alias;
	if (NULL != cur_column) {
		PACK_SQL_STATEMENT(column_alias->column, cur_column, column);
	} else {
		PACK_SQL_STATEMENT(column_alias->column, cur_cla, column_list_alias);
	}
	assert(table_alias_STATEMENT == table_alias_stmt->type);
	column_alias->table_alias_stmt = table_alias_stmt;
	OCTO_CMALLOC_STRUCT(column_list_alias, SqlColumnListAlias);
	if (NULL != cur_column) {
		column_list_alias->alias = cur_column->columnName;
	} else {
		column_list_alias->alias = cur_cla->alias;
	}
	/* Note: Unlike other callers of "get_sqlvaluetype_from_sqldatatype()", in this caller case, it is possible
	 * to see "UNKNOWN_SqlDataType" (in case of a VALUES() clause as we don't yet know the data type of the
	 * elements specified). The type will be later determined in "populate_data_type.c" so allow for an
	 * unknown type by passing "TRUE" as the second parameter below.
	 */
	if (NULL != cur_column) {
		column_list_alias->type = get_sqlvaluetype_from_sqldatatype(cur_column->data_type_struct.data_type, TRUE);
	} else {
		column_list_alias->type = cur_cla->type;
	}
	OCTO_CMALLOC_STRUCT(cur, SqlColumnList);
	dqinit(cur);
	cur->value = stmt;
	PACK_SQL_STATEMENT(column_list_alias->column_list, cur, column_list);
	dqinit(column_list_alias);
	return column_list_alias;
}

/* Adds an additional layer of column list alias structures to the column or column list alias list of nodes present in the
 * passed table_stmt.
 */
SqlColumnListAlias *get_encapsulated_cla_list(SqlStatement *table_stmt, SqlStatement *table_alias_stmt) {
	SqlStatement *column_or_cla_stmt = NULL;
	assert(NULL != table_stmt);
	switch (table_stmt->type) {
	case set_operation_STATEMENT:;
		SqlStatement *stmt = drill_to_table_alias(table_stmt);
		column_or_cla_stmt = stmt->v.table_alias->column_list;
		break;
	case table_alias_STATEMENT:
		column_or_cla_stmt = table_stmt->v.table_alias->column_list;
		break;
	case create_table_STATEMENT:
		column_or_cla_stmt = table_stmt->v.create_table->columns;
		break;
	case table_value_STATEMENT:
		column_or_cla_stmt = table_stmt->v.table_value->column_stmt;
		break;
	default:
		assert(FALSE);
		break;
	}
	SqlColumn *	    column;
	SqlColumnListAlias *cla;
	if (NULL == column_or_cla_stmt) {
		return NULL;
	} else if (column_STATEMENT == column_or_cla_stmt->type) {
		UNPACK_SQL_STATEMENT(column, column_or_cla_stmt, column);
		if (NULL == column) {
			return NULL;
		}
		cla = NULL;
	} else {
		assert(column_list_alias_STATEMENT == column_or_cla_stmt->type);
		UNPACK_SQL_STATEMENT(cla, column_or_cla_stmt, column_list_alias);
		if (NULL == cla) {
			return NULL;
		}
		column = NULL;
	}

	SqlColumnListAlias *cur_column_list_alias;
	SqlColumnListAlias *ret = NULL;
	if (NULL != column) {
		SqlColumn *cur_column, *start_column;
		cur_column = start_column = column;
		do {
			/* "cur_column->columnName" is NULL implies it is a table-level constraint stored as a column.
			 * Skip such columns as they are not user-visible columns.
			 */
			if (!cur_column->is_hidden_keycol && (NULL != cur_column->columnName)) {
				cur_column_list_alias = get_encapsulated_cla(cur_column, NULL, table_alias_stmt);
				if (NULL == ret) {
					ret = cur_column_list_alias;
				} else {
					dqappend(ret, cur_column_list_alias);
				}
			} else {
				/* Do not consider hidden key columns as part of user-visible column list of a table */
			}
			cur_column = cur_column->next;
		} while (cur_column != start_column);
	} else {
		assert(NULL != cla);
		SqlColumnListAlias *cur_cla, *start_cla;
		cur_cla = start_cla = cla;
		/* At present this code is used by table_reference.c to encapsulate a view's column list with
		 * `SqlColumnListAlias->SqlColumnList->SqlColumnAlias`. This helps to allow column references of the view to create
		 * a `SqlColumnAlias` using `sql_join->table_alias->column_list->SqlColumnListAlias`. The `SqlColumnAlias` created
		 * is stored in `sql_join->table_alias->column_list->SqlColumnListAlias->outer_query_column_alias`. This way the
		 * underlying view definition's `SqlColumnListAlias` is untouched. This enables having single view definition
		 * instead of creating a separate view definition for each instance of a view. This approach also helps during
		 * `LogicalPlan` creation to have a `unique_id` for the view which is separate from the underlying view definition's
		 * `unique_id`.
		 */
		do {
			cur_column_list_alias = get_encapsulated_cla(NULL, cur_cla, table_alias_stmt);
			if (NULL == ret) {
				ret = cur_column_list_alias;
			} else {
				dqappend(ret, cur_column_list_alias);
			}
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);
	}
	assert(NULL != ret);
	return ret;
}
