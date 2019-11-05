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

// Function invoked by the rule named "between_predicate" in src/parser.y
SqlStatement *between_predicate(SqlStatement *row_value_constructor, SqlStatement *from, SqlStatement *to, boolean_t not_specified)
{
	SqlStatement *left, *right, *ret;

	// Implement the BETWEEN operator which is just `(x >= from) AND (x <= to)`
	SQL_STATEMENT(left, binary_STATEMENT);
	MALLOC_STATEMENT(left, binary, SqlBinaryOperation);
	left->v.binary->operation = (!not_specified ? BOOLEAN_GREATER_THAN_OR_EQUALS : BOOLEAN_LESS_THAN);
	left->v.binary->operands[0] = row_value_constructor;
	left->v.binary->operands[1] = from;
	SQL_STATEMENT(right, binary_STATEMENT);
	MALLOC_STATEMENT(right, binary, SqlBinaryOperation);
	right->v.binary->operation = (!not_specified ? BOOLEAN_LESS_THAN_OR_EQUALS : BOOLEAN_GREATER_THAN);
	right->v.binary->operands[0] = row_value_constructor;
	right->v.binary->operands[1] = to;
	SQL_STATEMENT(ret, binary_STATEMENT);
	MALLOC_STATEMENT(ret, binary, SqlBinaryOperation);
	ret->v.binary->operation = (!not_specified ? BOOLEAN_AND : BOOLEAN_OR);
	ret->v.binary->operands[0] = left;
	ret->v.binary->operands[1] = right;
	return ret;
}
