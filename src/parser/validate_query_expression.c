/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by rules in "src/parser/insert_statement.c" and "sql_statement" rule in "src/parser.y"
 * This validates the query by invoking "qualify_query" and "populate_data_type".
 *
 * Returns: NULL in case of error
 *	non-NULL in case of success
 */
SqlStatement *validate_query_expression(SqlStatement *query_expression, ParseContext *parse_context, SqlStatementType cmd_type) {
	SqlValueType	      type;
	SqlStatement *	      ret;
	QualifyStatementParms ret_parms;
	int		      max_unique_id;

	/* cmd_type distinguishes various command types. Currently it only stores 2 values. SELECT or INSERT. */
	assert((select_STATEMENT == cmd_type) || (insert_STATEMENT == cmd_type) || (delete_from_STATEMENT == cmd_type));
	parse_context->command_tag = cmd_type;
	if (parse_context->abort) {
		return NULL;
	}
	ret = query_expression;
	ret_parms.ret_cla = NULL;
	ret_parms.max_unique_id = &max_unique_id;
	max_unique_id = 0; /* Need to initialize this to avoid garbage values from being read in "qualify_statement" */
	if (qualify_query(ret, NULL, NULL, &ret_parms)) {
		return NULL;
	}
	if (populate_data_type(ret, &type, parse_context)) {
		return NULL;
	}
	return ret;
}
