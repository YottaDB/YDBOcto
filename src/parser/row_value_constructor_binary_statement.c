/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

// Function invoked by various rules involving "row_value_constructor" in src/parser.y
// where the operand is to be treated as a boolean expression.
// We transform "(row_value_constructor)" usages to instead be "(row_value_constructor != 0)".
SqlStatement *row_value_constructor_binary_statement(SqlStatement *row_value_constructor)
{
	SqlStatement		*stmt;
	SqlBinaryOperation	*binary;
	SqlStatement		*zero;

	SQL_STATEMENT(stmt, binary_STATEMENT);
	MALLOC_STATEMENT(stmt, binary, SqlBinaryOperation);
	UNPACK_SQL_STATEMENT(binary, stmt, binary);
	binary->operation = BOOLEAN_NOT_EQUALS;
	SQL_STATEMENT(zero, value_STATEMENT);
	MALLOC_STATEMENT(zero, value, SqlValue);
	zero->v.value->type = INTEGER_LITERAL;
	zero->v.value->v.string_literal = "0";
	binary->operands[0] = row_value_constructor;
	binary->operands[1] = zero;
	return stmt;
}
