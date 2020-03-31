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

#define CREATE_BINARY_STATEMENT(STMT)					\
{									\
	SQL_STATEMENT(STMT, binary_STATEMENT);				\
	MALLOC_STATEMENT(STMT, binary, SqlBinaryOperation);		\
}

int regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, int is_regex_like_or_similar, int is_sensitive,
		int is_not, ParseContext *parse_context){
	SqlStatement 	*regex;
	int		status;

	if (is_not) {
		SQL_STATEMENT(*stmt, unary_STATEMENT);
		MALLOC_STATEMENT(*stmt, unary, SqlUnaryOperation);
		CREATE_BINARY_STATEMENT(regex);
		(*stmt)->v.unary->operation = BOOLEAN_NOT;
		(*stmt)->v.unary->operand = regex;
	} else {
		CREATE_BINARY_STATEMENT(*stmt);
		regex = *stmt;
	}

	if (is_sensitive) {
		if (1 == is_regex_like_or_similar) {
			regex->v.binary->operation = BOOLEAN_REGEX_SENSITIVE_LIKE;
		} else if (2 == is_regex_like_or_similar) {
			regex->v.binary->operation = BOOLEAN_REGEX_SENSITIVE_SIMILARTO;
		} else {
			regex->v.binary->operation = BOOLEAN_REGEX_SENSITIVE;
		}
	} else {
		if (1 == is_regex_like_or_similar) {
			regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE_LIKE;
		} else if (2 == is_regex_like_or_similar) {
			regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE_SIMILARTO;
		} else {
			regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE;
		}
	}
	regex->v.binary->operands[0] = op0;
	/* Parse literals to parameters only if input is of type STRING_LITERAL.
	 * This avoids multiple plan generation for different pattern strings.
	 */
	status = 0;
	if (0 == is_regex_like_or_similar) {
		if ((value_STATEMENT == op1->type) && (STRING_LITERAL == op1->v.value->type)) {
			SqlValue	*value;

			UNPACK_SQL_STATEMENT(value, op1, value);
			status = parse_literal_to_parameter(parse_context, value, TRUE);
		}
	} else if (value_STATEMENT == op1->type) {
		/* LIKE and SIMILAR TO OPERATOR */
		SqlValue	*value;

		UNPACK_SQL_STATEMENT(value, op1, value);
		if (STRING_LITERAL == value->type) {
			status = parse_literal_to_parameter(parse_context, value, TRUE);
		}
	}
	regex->v.binary->operands[1] = op1;
	return ((0 != status) ? 1 : 0);
}
