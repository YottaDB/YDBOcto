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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

// Function invoked by the rules named "non_join_query_expression" and "non_join_query_term" in src/parser.y
SqlStatement *set_operation(enum SqlSetOperationType setoper_type, SqlStatement *left_operand, SqlStatement *right_operand) {
	SqlSetOperation *set_oper;
	SqlStatement *	 ret;

	SQL_STATEMENT(ret, set_operation_STATEMENT);
	MALLOC_STATEMENT(ret, set_operation, SqlSetOperation);
	UNPACK_SQL_STATEMENT(set_oper, ret, set_operation);
	set_oper->type = setoper_type;
	set_oper->operand[0] = left_operand;
	set_oper->operand[1] = right_operand;
	return ret;
}
