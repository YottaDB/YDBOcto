/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by src/parser/table_reference.c and src/parser/derived_table.c.
 * "correlation_specification" is a pointer to a table name alias and an optional list of column name aliases.
 *
 * Returns:
 *	0 in case of success.
 *	1 in case of errors so caller can take appropriate action.
 */
int copy_correlation_specification_aliases(SqlTableAlias *table_alias, SqlStatement *correlation_specification) {
	SqlColumnList *	    table_name_alias, *column_name_alias;
	SqlColumnListAlias *start_cla, *cur_cla;
	int		    table_columns, correlation_columns;

	assert(NULL != correlation_specification);
	UNPACK_SQL_STATEMENT(table_name_alias, correlation_specification, column_list);
	/* Set the table name alias */
	table_alias->alias = table_name_alias->value;
	/* Set column name aliases */
	column_name_alias = table_name_alias->next; /* this points to the beginning of the list of column name aliases */
	UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
	cur_cla = start_cla;
	table_columns = 0;
	/* Note that the list of column name aliases will circle back to the table name alias at the beginning of the list
	 * and so that is how one detects the end of the linked list below.
	 */
	correlation_columns = 0;
	while (column_name_alias != table_name_alias) {
		cur_cla->alias = column_name_alias->value;
		correlation_columns++;
		column_name_alias = column_name_alias->next;
		table_columns++;
		cur_cla = cur_cla->next;
		if (start_cla == cur_cla) {
			if (column_name_alias != table_name_alias) {
				SqlValue *value;

				/* More column name aliases have been specified than there are table columns. Issue error. */
				do {
					correlation_columns++;
					column_name_alias = column_name_alias->next;
				} while (column_name_alias != table_name_alias);
				UNPACK_SQL_STATEMENT(value, table_alias->alias, value);
				ERROR(ERR_AS_MORE_COLUMNS, value->v.string_literal, table_columns, correlation_columns);
				yyerror(NULL, NULL, &table_alias->alias, NULL, NULL, NULL);
				return 1;
			}
			break;
		}
	}
	return 0;
}
