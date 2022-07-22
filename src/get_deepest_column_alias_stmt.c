/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/
#include "octo.h"
#include "octo_types.h"
#include <assert.h>

#ifndef NDEBUG
/* Following function validates that the argument `deepest_column_alias_stmt` has a common ancestor with `other_column_alias_stmt`.
 * This ancestry is found by looking at `parent_table_alias->unique_id` of both `column_alias`s.
 */
static void has_common_ancestor(SqlStatement *deepest_column_alias_stmt, SqlStatement *other_column_alias_stmt) {
	SqlColumnAlias *deepest_column_alias;
	UNPACK_SQL_STATEMENT(deepest_column_alias, deepest_column_alias_stmt, column_alias);

	SqlTableAlias *deepest_table_alias;
	UNPACK_SQL_STATEMENT(deepest_table_alias, deepest_column_alias->table_alias_stmt, table_alias);

	SqlColumnAlias *other_column_alias;
	UNPACK_SQL_STATEMENT(other_column_alias, other_column_alias_stmt, column_alias);

	SqlTableAlias *other_table_alias;
	UNPACK_SQL_STATEMENT(other_table_alias, other_column_alias->table_alias_stmt, table_alias);

	boolean_t common_ancestor_reached = FALSE;
	UNUSED(common_ancestor_reached); // Avoids [-Wunused-but-set-variable] for the above initialization
	while (NULL != deepest_table_alias) {
		if (other_table_alias->parent_table_alias->unique_id == deepest_table_alias->parent_table_alias->unique_id) {
			common_ancestor_reached = TRUE;
			break;
		}
		deepest_table_alias = deepest_table_alias->parent_table_alias;
	}
	assert(TRUE == common_ancestor_reached);
}
#endif

/* Among `first_column_alias_stmt` and `second_column_alias_stmt`, below function returns the column_alias_stmt which belongs
 * to the deepest query level. If both the operands belong to the same query level, `second_column_alias_stmt` is returned as the
 * deepest_column_alias_stmt.
 */
SqlStatement *get_deepest_column_alias_stmt(SqlStatement *first_column_alias_stmt, SqlStatement *second_column_alias_stmt) {
	SqlColumnAlias *first_column_alias, *second_column_alias;
	UNPACK_SQL_STATEMENT(first_column_alias, first_column_alias_stmt, column_alias);
	UNPACK_SQL_STATEMENT(second_column_alias, second_column_alias_stmt, column_alias);

	SqlTableAlias *first_table_alias, *second_table_alias;
	UNPACK_SQL_STATEMENT(first_table_alias, first_column_alias->table_alias_stmt, table_alias);
	UNPACK_SQL_STATEMENT(second_table_alias, second_column_alias->table_alias_stmt, table_alias);

	// The column alias whose parent_table_alias has the least unique_id is considered the deepest query level column alias
	SqlStatement *deepest_column_alias_stmt;
	if (second_table_alias->parent_table_alias->unique_id <= first_table_alias->parent_table_alias->unique_id) {
		deepest_column_alias_stmt = second_column_alias_stmt;
#ifndef NDEBUG
		if (second_table_alias->parent_table_alias->unique_id < first_table_alias->parent_table_alias->unique_id) {
			has_common_ancestor(deepest_column_alias_stmt, first_column_alias_stmt);
		}
#endif

	} else {
		deepest_column_alias_stmt = first_column_alias_stmt;
#ifndef NDEBUG
		has_common_ancestor(deepest_column_alias_stmt, second_column_alias_stmt);
#endif
	}
	return deepest_column_alias_stmt;
}
