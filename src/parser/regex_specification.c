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

void regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, int is_regex_like_or_similar, int is_sensitive, int is_not){
	SqlStatement *regex;
	if (is_not) {
		SQL_STATEMENT(*stmt, unary_STATEMENT);
		MALLOC_STATEMENT(*stmt, unary, SqlUnaryOperation);
		SQL_STATEMENT(regex, binary_STATEMENT);
		MALLOC_STATEMENT(regex, binary, SqlBinaryOperation);
		(*stmt)->v.unary->operation = BOOLEAN_NOT;
		(*stmt)->v.unary->operand = regex;
	} else {
		SQL_STATEMENT(*stmt, binary_STATEMENT);
		MALLOC_STATEMENT(*stmt, binary, SqlBinaryOperation);
		regex = *stmt;
	}

	if (is_sensitive) {
		regex->v.binary->operation = BOOLEAN_REGEX_SENSITIVE;
	} else {
		regex->v.binary->operation = BOOLEAN_REGEX_INSENSITIVE;
	}
	regex->v.binary->operands[0] = op0;
	if (0 == is_regex_like_or_similar) {
		trim_dot_star(op1->v.value);
		regex->v.binary->operands[1] = op1;
	} else if (0 < is_regex_like_or_similar) {
		SqlValue *value;
		UNPACK_SQL_STATEMENT(value, op1, value);
		if(COERCE_TYPE == value->type){
			value->v.coerce_target->v.value->v.string_literal = (1 == is_regex_like_or_similar ?
				like_to_regex(value->v.coerce_target->v.value->v.string_literal) : similar_to_regex(value->v.coerce_target->v.value->v.string_literal));
			trim_dot_star(value->v.coerce_target->v.value);
		} else {
			value->v.string_literal = (1 == is_regex_like_or_similar  ? like_to_regex(value->v.string_literal) : similar_to_regex(value->v.string_literal));
			trim_dot_star(value);
		}
		regex->v.binary->operands[1] = op1;
	}
}

