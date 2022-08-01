/****************************************************************
 *								*
 * Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Helper function used to fix the type information of a column list involving SET operations.
 * "fix_type" points to a cla list that is used to derive the type to fix.
 */
int populate_data_type_cla_fix(SqlStatement *v, ParseContext *parse_context, SqlColumnListAlias *fix_type_cla) {
	int result;

	result = 0;
	assert(NULL != v);
	switch (v->type) {
	case set_operation_STATEMENT:;
		SqlSetOperation *set_operation;

		UNPACK_SQL_STATEMENT(set_operation, v, set_operation);
		result |= populate_data_type_cla_fix(set_operation->operand[0], parse_context, fix_type_cla);
		assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
		UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
		result |= populate_data_type_cla_fix(set_operation->operand[1], parse_context, fix_type_cla);
		assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
		break;
	case table_alias_STATEMENT:;
		SqlTableAlias *table_alias;
		SqlValueType   dummy_type;

		UNPACK_SQL_STATEMENT(table_alias, v, table_alias);
		if (select_STATEMENT != table_alias->table->type) {
			break;
		}
		result |= populate_data_type_column_list_alias(table_alias->column_list, &dummy_type, NULL, TRUE, parse_context,
							       fix_type_cla);
		assert(!result);
		break;
	default:
		break;
	}
	return result;
}
