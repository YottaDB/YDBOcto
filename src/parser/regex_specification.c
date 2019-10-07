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

int regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, int is_regex_like_or_similar, int is_sensitive, int is_not, char *cursorId){
	SqlStatement *regex;
	char *c;
	int is_dot_star = TRUE;
	SqlValue *value;
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
			int status = parse_literal_to_parameter(cursorId, value, TRUE);
			if (0 != status) {
				OCTO_CFREE(memory_chunks);
				return 1;
			}
		}
		regex->v.binary->operands[1] = op1;
	}

	/* check if the converted regex is just '.*' and if so replace it with boolean TRUE */
	value = regex->v.binary->operands[1]->v.value;
	c = value->v.string_literal;
	if ('^' == *c) {
		c++;
	}
	if (('.' == *c) && ('*' == *(c+1))) {
		c += 2;
	} else {
		is_dot_star = FALSE;
	}
	if ('$' == *c) {
		c++;
	}
	if('\0' == *c && is_dot_star){
		int status;
		SQL_STATEMENT(regex, value_STATEMENT);
		MALLOC_STATEMENT(regex, value, SqlValue);
		regex->type = value_STATEMENT;
		regex->v.value->type = BOOLEAN_VALUE;
		// Convert regex to simple boolean value as ".*" is all or nothing
		// No need to differentiate based on is_not here, since the NOT case is handled below
		regex->v.value->v.string_literal = "1";
		status = parse_literal_to_parameter(cursorId, regex->v.value, FALSE);
		if (0 != status) {
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		// Retain unary_STATEMENT generated above to correctly hash is vs is_not cases to separate plans
		// Since the unary_STATEMENT is set to BOOLEAN_NOT in the is_not case above, the negation is tracked by the logical
		// plan rather than the regex literal value
		if (is_not) {
			(*stmt)->v.unary->operand = regex;
		} else {
			(*stmt) = regex;
		}
	}
	return 0;
}

