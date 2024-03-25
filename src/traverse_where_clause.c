/****************************************************************
 *								*
 * Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	*
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

SqlStatement *traverse_where_clause(SqlStatement *binary_stmt, SqlJoin *start_join) {
	SqlStatement	   *ret;
	SqlBinaryOperation *binary;
	int		    i;

	assert(binary_STATEMENT == binary_stmt->type);
	UNPACK_SQL_STATEMENT(binary, binary_stmt, binary);
	assert(BOOLEAN_AND == binary->operation);
	for (i = 0; i < 2; i++) {
		SqlStatement *child_stmt;

		child_stmt = binary->operands[i];
		if (IS_STMT_BOOLEAN_AND(child_stmt)) {
			binary->operands[i] = traverse_where_clause(child_stmt, start_join);
		} else {
			move_where_clause_to_on_clause(&binary->operands[i], start_join);
		}
	}
	ret = NULL;
	if (NULL != binary->operands[0]) {
		ret = binary->operands[0];
	}
	if (NULL != binary->operands[1]) {
		ret = ((NULL != ret) ? binary_stmt : binary->operands[1]);
	}
	return ret;
}
