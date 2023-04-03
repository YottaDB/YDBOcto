/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Returns the SqlValueType of the passed column alias statement. The function depends on
 * `set_oper_stmt` and its `col_type_list_stmt` to identify the type instead of deriving the
 * type from the `column` field. This is because `col_type_list_stmt` is set with type information
 * after looking through column values in all rows of the set_operation. This is neccessary as
 * we don't want to assume the type of the column by looking at just one row.
 * NOTE: This function is expected to be called only for a set_operation column alias.
 */
SqlValueType get_set_operation_column_alias_type(SqlStatement *ca_stmt) {
	SqlColumnAlias *left_ca;
	UNPACK_SQL_STATEMENT(left_ca, ca_stmt, column_alias);
	assert(NULL != left_ca->set_oper_stmt);

	SqlTableAlias *table_alias;
	UNPACK_SQL_STATEMENT(table_alias, left_ca->table_alias_stmt, table_alias);

	SqlColumnListAlias *input_cla;
	UNPACK_SQL_STATEMENT(input_cla, left_ca->column, column_list_alias);
	int column_num = get_column_number_from_column_list_alias(input_cla, table_alias);

	SqlSetOperation *set_oper;
	UNPACK_SQL_STATEMENT(set_oper, left_ca->set_oper_stmt, set_operation);

	SqlColumnListAlias *start_cla, *cur_cla;
	start_cla = cur_cla = (NULL != set_oper->col_type_list_stmt) ? set_oper->col_type_list_stmt->v.column_list_alias : NULL;
	// Following if block avoids [clang-analyzer-core.NullDereference] error
	// We do not expect it to have NULL value
	assert(NULL != cur_cla);
	if (NULL == cur_cla) {
		return UNKNOWN_SqlValueType;
	}
	int column_counter = 1;
	do {
		if (column_num == column_counter) {
			return cur_cla->type;
		}
		cur_cla = cur_cla->next;
		column_counter++;
	} while (start_cla != cur_cla);
	assert(FALSE);
	return UNKNOWN_SqlValueType;
}
