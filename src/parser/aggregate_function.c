/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

// Function invoked by the rule named "set_function_specification" and "general_set_function" in src/parser.y
SqlStatement *aggregate_function(SqlAggregateType aggregate_type, OptionalKeyword set_quantifier, SqlStatement *value_expression,
				 YYLTYPE *loc) {
	SqlStatement *	      ret, *aggregate_stmt;
	SqlAggregateFunction *af;
	SqlValue *	      value;

	SQL_STATEMENT(ret, value_STATEMENT);
	MALLOC_STATEMENT(ret, value, SqlValue);
	UNPACK_SQL_STATEMENT(value, ret, value);
	value->type = CALCULATED_VALUE;

	SQL_STATEMENT(aggregate_stmt, aggregate_function_STATEMENT);
	MALLOC_STATEMENT(aggregate_stmt, aggregate_function, SqlAggregateFunction);
	UNPACK_SQL_STATEMENT(af, aggregate_stmt, aggregate_function);
	value->v.calculated = aggregate_stmt;
	if (OPTIONAL_DISTINCT == set_quantifier) {
		assert(AGGREGATE_COUNT_ASTERISK != aggregate_type);
		assert(AGGREGATE_COUNT <= aggregate_type);
		assert(AGGREGATE_MAX >= aggregate_type);
		assert((AGGREGATE_MAX + 1) == AGGREGATE_COUNT_DISTINCT);
		assert((AGGREGATE_AVG_DISTINCT - AGGREGATE_AVG) == (AGGREGATE_COUNT_DISTINCT - AGGREGATE_COUNT));
		assert((AGGREGATE_SUM_DISTINCT - AGGREGATE_SUM) == (AGGREGATE_COUNT_DISTINCT - AGGREGATE_COUNT));
		if ((AGGREGATE_MIN == aggregate_type) || (AGGREGATE_MAX == aggregate_type)) {
			/* MIN(DISTINCT) and MAX(DISTINCT) are equivalent to MIN and MAX respectively */
			af->type = aggregate_type;
		} else {
			af->type = aggregate_type + (AGGREGATE_COUNT_DISTINCT - AGGREGATE_COUNT);
		}
	} else {
		af->type = aggregate_type;
	}
	af->parameter = create_sql_column_list(value_expression, NULL, loc);
	return ret;
}
