/****************************************************************
 *								*
 * Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>
#include <string.h>

#include "octo.h"

/* Validates the ITERATOR keyword and issues errors as appropriate.
 *
 * Two shapes of ITERATOR value are accepted:
 *   1. "$$tag^routine"
 *      Bare M entryref. Octo auto-appends every key value at codegen time.
 *   2. "$$tag^routine(arglist)"
 *      Entryref followed by a user-supplied argument list. Each `keys("colname")`
 *      token inside `arglist` is replaced at codegen time by the M expression
 *      for that column's current key value. Any non-`keys()` bytes pass through
 *      verbatim, allowing literals and other M expressions to be passed to the
 *      iterator routine. `values(...)` is disallowed here, because the iterator
 *      runs before any row context is available.
 *
 * Returns
 *   -1 in case validation failed.
 *    0 otherwise (i.e. success).
 */
int validate_iterator_keyword(SqlColumn *cur_column, SqlTable *table) {
	SqlValue *	    columnName;
	SqlOptionalKeyword *cur_keyword, *start_keyword;

	/* Constraint #1: ITERATOR is meaningful only on a primary-key column. A non-key
	 * column has no subscript slot for Octo to iterate over.
	 */
	if (!IS_KEY_COLUMN(cur_column)) {
		assert(NULL != cur_column->columnName);
		UNPACK_SQL_STATEMENT(columnName, cur_column->columnName, value);
		ERROR(ERR_ITERATOR_CANNOT_BE_NON_KEY_COLUMN, columnName->v.string_literal);
		return -1;
	}

	/* Constraint #2: ITERATOR cannot coexist with START/END/STARTINCLUDE/ENDPOINT.
	 * Those keywords describe a half-open subscript range that Octo would scan with
	 * $ORDER; ITERATOR replaces that scan with a user-defined function, so the
	 * range bounds would have no meaning.
	 */
	UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
	cur_keyword = start_keyword;
	do {
		switch (cur_keyword->keyword) {
		case OPTIONAL_START:
		case OPTIONAL_END:
		case OPTIONAL_STARTINCLUDE:
		case OPTIONAL_ENDPOINT:;
			assert(NULL != cur_column->columnName);
			UNPACK_SQL_STATEMENT(columnName, cur_column->columnName, value);
			ERROR(ERR_ITERATOR_CANNOT_BE_COMBINED, columnName->v.string_literal);
			return -1;
		default:
			break;
		}
		cur_keyword = cur_keyword->next;
	} while (cur_keyword != start_keyword);

	/* Constraint #3: validate the string format and (if a parenthesized argument list
	 * is present) walk it to confirm every `keys("colname")` reference resolves to a
	 * KEY column of this table, and no `values(...)` appears.
	 */
	SqlOptionalKeyword *iterator_keyword;
	SqlValue	   *iterator_value;
	char		   *iterator;
	char		   *paren;

	iterator_keyword = get_keyword(cur_column, OPTIONAL_ITERATOR);
	assert(NULL != iterator_keyword);
	UNPACK_SQL_STATEMENT(iterator_value, iterator_keyword->v, value);
	iterator = iterator_value->v.string_literal;
	paren = strchr(iterator, '(');

	if (NULL == paren) {
		/* No parenthesized argument list: the legacy "$$tag^routine" format.
		 * Codegen will auto-append all keys; nothing more to validate here.
		 */
		return 0;
	}

	size_t iterator_length = strlen(iterator);

	if ((paren == iterator) || (')' != iterator[iterator_length - 1]) || ((paren + 1) == (iterator + iterator_length - 1))) {
		/* Reject shapes that cannot be "$$tag^routine(arglist)":
		 *   - empty prefix before the '(' (i.e. the string *starts* with '(')
		 *   - anything trailing the closing ')'
		 *   - empty argument list "$$tag^routine()" -- if the user adds the parens,
		 *     they must supply at least one argument; otherwise the bare entryref
		 *     form is what they want.
		 */
		ERROR(ERR_ITERATOR_BAD_SYNTAX, "");
		return -1;
	}

	/* Walk the argument-list bytes between '(' and the final ')'.
	 * The scan is modelled on `validate_global_keyword.c` -- same `match_expression()`
	 * helper, same prev-character convention so `keys(` / `values(` are recognised only
	 * after '(' or ','.
	 */
	char *ptr_start = paren + 1;
	char *ptr_end = iterator + iterator_length - 1; /* Points at the trailing ')'. */
	char *ptr = ptr_start;

	while (ptr < ptr_end) {
		char		    prev;
		char		    column[OCTO_MAX_IDENT + 1];
		int		    expr_len;
		ExpressionMatchType match;

		/* At "ptr_start" the byte before is the iterator's opening '(' (since
		 * "ptr_start = paren + 1"), so "*(ptr - 1)" would also evaluate to '('
		 * there. The ternary makes that intent explicit without requiring the
		 * reader to verify the off-by-one. Unlike "tmpl_emit_source.ctemplate"
		 * (where "source_ptr" can be the first byte of the input string and
		 * "*(source_ptr - 1)" would be out-of-bounds), we always have a real
		 * preceding byte available, so no synthetic 'k' / 'v' special-case is
		 * needed.
		 */
		prev = (ptr == ptr_start) ? '(' : *(ptr - 1);
		match = match_expression(ptr, column, &expr_len, sizeof(column), prev);

		if (NoMatchExpression == match) {
			/* Opaque byte (part of an M literal, expression, or whitespace).
			 * Skip it; nothing to validate.
			 */
			ptr++;
			continue;
		}

		if (KeysExpression == match) {
			SqlColumn *referenced_column;

			referenced_column = find_column(column, table);
			if (NULL == referenced_column) {
				/* Column referenced by `keys("colname")` does not exist on this table. */
				ERROR(ERR_UNKNOWN_COLUMN_NAME, column);
				return -1;
			}
			if (!IS_KEY_COLUMN(referenced_column)) {
				/* Column exists but is not a KEY column, so it has no subscript value
				 * for Octo to substitute at codegen time.
				 */
				ERROR(ERR_ITERATOR_KEYS_NEEDS_A_KEY_COLUMN, column);
				return -1;
			}
		} else if (ValuesExpression == match) {
			/* `values()` references a row's column value, which is not available when
			 * the iterator runs -- it executes before any row has been fetched.
			 */
			ERROR(ERR_ITERATOR_VALUES_NOT_ALLOWED, "");
			return -1;
		} else {
			assert(MatchExpressionOFlow == match);
			/* `keys("...")` column name exceeded OCTO_MAX_IDENT. Treat as bad syntax. */
			ERROR(ERR_ITERATOR_BAD_SYNTAX, "");
			return -1;
		}

		assert(0 < expr_len);
		ptr += expr_len;
	}

	return 0;
}
