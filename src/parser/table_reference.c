/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by the rule named "table_reference" in src/parser/select.y
 *
 * Returns:
 *	non-NULL pointer in case of success.
 *	NULL     pointer in case of errors so caller can take appropriate action.
 */

SqlStatement *table_reference(SqlStatement *column_name, SqlStatement *correlation_specification, int *plan_id) {
	SqlStatement * ret, *tableName;
	SqlValue *     value;
	SqlJoin *      join;
	SqlStatement * table_stmt;
	SqlColumn *    column;
	SqlTableAlias *table_alias;

	switch (column_name->type) {
	case value_STATEMENT:
		/* column_name holds the name of the desired table */
		UNPACK_SQL_STATEMENT(value, column_name, value);
		table_stmt = find_view_or_table(value->v.reference);
		if (NULL == table_stmt) {
			ERROR(ERR_UNKNOWN_TABLE, value->v.reference);
			yyerror(NULL, NULL, &column_name, NULL, NULL, NULL);
			return NULL;
		}
		break;
	default:
		assert(table_value_STATEMENT == column_name->type);
		/* Caller is "table_value_constructor" rule in parser.y.
		 * In this case, it is not a pre-existing table. But a table defined by the VALUES clause.
		 */
		table_stmt = NULL;
		break;
	}
	SQL_STATEMENT(ret, join_STATEMENT);
	MALLOC_STATEMENT(ret, join, SqlJoin);
	join = ret->v.join;
	SQL_STATEMENT(join->value, table_alias_STATEMENT);
	MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(table_alias, join->value, table_alias);
	if (NULL != table_stmt) {
		if (create_view_STATEMENT == table_stmt->type) {
			SqlView *view;
			UNPACK_SQL_STATEMENT(view, table_stmt, create_view);
			SQL_STATEMENT_FROM_SQLTABLE_OR_SQLVIEW(table_alias, view);
			tableName = view->viewName;
			table_stmt = view->src_table_alias_stmt;
		} else {
			SqlTable *table;
			UNPACK_SQL_STATEMENT(table, table_stmt, create_table);
			SQL_STATEMENT_FROM_SQLTABLE_OR_SQLVIEW(table_alias, table);
			tableName = table->tableName;
		}
	} else {
		SqlStatement * table_value_stmt;
		SqlTableValue *table_value;
		SqlRowValue *  row_value;
		SqlColumn *    start_column;
		int	       i, num_columns;

		table_value_stmt = column_name;
		table_alias->table = table_value_stmt;
		/* Construct a list of SqlColumn structures with column names "column1", "column2" etc. filled in */
		UNPACK_SQL_STATEMENT(table_value, table_value_stmt, table_value);
		assert(NULL == correlation_specification);
		UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
		num_columns = row_value->num_columns;
		assert(num_columns);
		start_column = NULL;
		for (i = 1; i <= num_columns; i++) {
			SqlStatement *column_name_stmt;
			SqlValue *    value;
			int	      column_name_len;

			OCTO_CMALLOC_STRUCT(column, SqlColumn);
			column->table = table_value_stmt;
			column->column_number = i;
			assert(UNKNOWN_SqlDataType == column->data_type_struct.data_type);
			column->data_type_struct.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;
			column->data_type_struct.scale = SCALE_UNSPECIFIED;
			column->data_type_struct.size_or_precision_parameter_index = 0;
			column->data_type_struct.scale_parameter_index = 0;
			SQL_STATEMENT(column_name_stmt, value_STATEMENT);
			column->columnName = column_name_stmt;
			MALLOC_STATEMENT(column_name_stmt, value, SqlValue);
			UNPACK_SQL_STATEMENT(value, column_name_stmt, value);
			value->type = COLUMN_REFERENCE;
			/* Column name is "column1", "column2" etc. where the number at the end can be a maximum of
			 * INT32_TO_STRING_MAX bytes long.
			 */
			column_name_len = sizeof(VALUES_COLUMN_NAME_PREFIX) + INT32_TO_STRING_MAX;
			value->v.string_literal = octo_cmalloc(memory_chunks, column_name_len);
			sprintf(value->v.string_literal, "%s%d", VALUES_COLUMN_NAME_PREFIX, i);
			dqinit(column);
			if (NULL == start_column) {
				start_column = column;
			} else {
				dqappend(start_column, column);
			}
		}
		column = start_column;
		SQL_STATEMENT(table_value->column_stmt, column_STATEMENT);
		table_value->column_stmt->v.column = column;
		tableName = NULL;
		table_stmt = table_value_stmt;
	}
	PACK_SQL_STATEMENT(table_alias->column_list, get_encapsulated_cla_list(table_stmt, join->value), column_list_alias);
	if (NULL != correlation_specification) {
		SqlColumnList *column_list;

		UNPACK_SQL_STATEMENT(column_list, correlation_specification, column_list);
		table_alias->alias = column_list->value;
		table_alias->correlation_specification = correlation_specification;
		/* Defer processing column name aliases till all asterisk or table.* usage is expanded in qualify_query() */
	} else {
		table_alias->alias = tableName;
	}
	/* We can probably put a variable in the bison local for this */
	table_alias->unique_id = *plan_id;
	(*plan_id)++;
	dqinit(join);
	join->max_unique_id = config->plan_id;
	return ret;
}
