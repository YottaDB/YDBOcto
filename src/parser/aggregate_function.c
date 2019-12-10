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

// Function invoked by the rule named "set_function_specification" and "general_set_function" in src/parser.y
SqlStatement *aggregate_function(SqlAggregateType aggregate_type, OptionalKeyword set_quantifier, SqlStatement *value_expression)
{
	SqlStatement		*ret, *aggregate_stmt, *parameter_stmt;
	SqlAggregateFunction	*af;
	SqlValue		*value;
	SqlColumnList		*column_list;

	SQL_STATEMENT(ret, value_STATEMENT);
	MALLOC_STATEMENT(ret, value, SqlValue);
	UNPACK_SQL_STATEMENT(value, ret, value);
	value->type = CALCULATED_VALUE;

	SQL_STATEMENT(aggregate_stmt, aggregate_function_STATEMENT);
	MALLOC_STATEMENT(aggregate_stmt, aggregate_function, SqlAggregateFunction);
	UNPACK_SQL_STATEMENT(af, aggregate_stmt, aggregate_function);
	if (OPTIONAL_DISTINCT == set_quantifier) {
		assert(COUNT_ASTERISK_AGGREGATE != aggregate_type);
		assert(COUNT_AGGREGATE <= aggregate_type);
		assert(MAX_AGGREGATE >= aggregate_type);
		assert((MAX_AGGREGATE + 1) == COUNT_AGGREGATE_DISTINCT);
		assert((AVG_AGGREGATE_DISTINCT - AVG_AGGREGATE) == (COUNT_AGGREGATE_DISTINCT - COUNT_AGGREGATE));
		assert((SUM_AGGREGATE_DISTINCT - SUM_AGGREGATE) == (COUNT_AGGREGATE_DISTINCT - COUNT_AGGREGATE));
		if ((MIN_AGGREGATE == aggregate_type) || (MAX_AGGREGATE == aggregate_type)) {
			/* MIN(DISTINCT) and MAX(DISTINCT) are equivalent to MIN and MAX respectively */
			af->type = aggregate_type;
		} else {
			af->type = aggregate_type + (COUNT_AGGREGATE_DISTINCT - COUNT_AGGREGATE);
		}
	} else {
		af->type = aggregate_type;
	}
	SQL_STATEMENT(parameter_stmt, column_list_STATEMENT);
	MALLOC_STATEMENT(parameter_stmt, column_list, SqlColumnList);
	UNPACK_SQL_STATEMENT(column_list, parameter_stmt, column_list);
	column_list->value = value_expression;
	dqinit(column_list);
	if (NULL != value_expression) {
		parameter_stmt->loc = value_expression->loc;	/* Cannot use "yyloc" here so passing it from parser
								 * through value_expression->loc.
								 */
	}
	af->parameter = parameter_stmt;
	value->v.calculated = aggregate_stmt;
	return ret;
}
