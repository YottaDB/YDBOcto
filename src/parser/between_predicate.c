/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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
SqlStatement *between_predicate(SqlStatement *row_value_constructor, SqlStatement *from, SqlStatement *to,
				boolean_t not_specified) {
	SqlStatement *left, *right, *ret;

	/* Implement the BETWEEN operator which is just `(x >= from) AND (x <= to)`.
	 * But note that we should not repeat use the same `x` in the binary operations on either side of the AND.
	 * This is because a `qualify_statement()` call for the `x` on the left side of the AND would then change what
	 * was a value_STATEMENT(COLUMN_REFERENCE) type to a column_alias_STATEMENT type and set `max_unique_id` appropriately
	 * (which is used by the "move_where_clause_to_on_clause()" optimization). If the `x` was repeated on the right side
	 * of the AND, the `qualify_statement()` call for the `x` on the right side of the AND would see it as a
	 * column_alias_STATEMENT and return right away without doing `max_unique_id` processing which would then confuse
	 * the "move_where_clause_to_on_clause()" optimization to return incorrect results (YDBOcto#843).
	 * Hence the need to take a copy of "row_value_constructor" below.
	 */
	SQL_STATEMENT(left, binary_STATEMENT);
	MALLOC_STATEMENT(left, binary, SqlBinaryOperation);
	left->v.binary->operation = (!not_specified ? BOOLEAN_GREATER_THAN_OR_EQUALS : BOOLEAN_LESS_THAN);
	left->v.binary->operands[0] = row_value_constructor;
	left->v.binary->operands[1] = from;
	SQL_STATEMENT(right, binary_STATEMENT);
	MALLOC_STATEMENT(right, binary, SqlBinaryOperation);
	right->v.binary->operation = (!not_specified ? BOOLEAN_LESS_THAN_OR_EQUALS : BOOLEAN_GREATER_THAN);
	right->v.binary->operands[0] = copy_sql_statement(row_value_constructor);
	right->v.binary->operands[1] = to;
	SQL_STATEMENT(ret, binary_STATEMENT);
	MALLOC_STATEMENT(ret, binary, SqlBinaryOperation);
	ret->v.binary->operation = (!not_specified ? BOOLEAN_AND : BOOLEAN_OR);
	ret->v.binary->operands[0] = left;
	ret->v.binary->operands[1] = right;
	return ret;
}
