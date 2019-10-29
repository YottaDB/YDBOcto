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

#include <stdio.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#include "parser.h"

void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, char const *s)
{
	UNUSED(plan_id);
	if ((NULL == scan) && (NULL != out)) {
		/* This is a "yyerror" call from outside the parser (e.g. "populate_data_type.c").
		 * In this case, compute "llocp" from "out".
		 */
		SqlColumnListAlias	*cur_cla;
		SqlSetOperation		*set_operation;
		SqlStatement		*sql_stmt, *stmt;
		SqlTableAlias		*table_alias;

		assert(NULL == llocp);
		stmt = *out;
		if (set_operation_STATEMENT == stmt->type) {
			UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
			sql_stmt = drill_to_table_alias(set_operation->operand[0]);
		} else if (table_alias_STATEMENT == stmt->type) {
			sql_stmt = stmt;
		} else
			sql_stmt = NULL;
		if (NULL != sql_stmt) {
			UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
			assert(NULL != table_alias->column_list);
			UNPACK_SQL_STATEMENT(cur_cla, table_alias->column_list, column_list_alias);
			llocp = &cur_cla->column_list->loc;
		} else {
			llocp = &stmt->loc;
		}
	}
	if (llocp->first_line || llocp->first_column || llocp->last_column) {
		fprintf(err_buffer, "Error with syntax near (line %d, column %d):", llocp->first_line + 1, llocp->first_column);
		print_yyloc(llocp);
		if (NULL != s)
			fprintf(err_buffer, "%s\n", s);
	} else {
		assert(NULL == s);
	}
}

void print_yyloc(YYLTYPE *llocp) {
	// llocp is 0 based
	int cur_line = 0, cur_column = 0, offset = old_input_index;
	char *c, *line_begin = input_buffer_combined, *line_end, *issue_line, old_terminator;

	/* start at begining of buffer
	 * if a newline is found before the old_input_index (a multiline query)
	 * move the start of the string, and shrink the offest
	 */
	c = line_begin;
	for (; ('\0' != *c) && (cur_column < old_input_index); c++, cur_column++) {
		if ('\n' == *c) {
			line_begin = c + 1;
			/* shift the offset over */
			offset = old_input_index - cur_column - 1;
		}
	}
	cur_column = 0;

	/* if a newline is found in this loop then offset is -1 (-1 not 0 due to spacing)
	 * because no previous query could be on that line
	 */
	c = line_begin;
	for (; ('\0' != *c) && (cur_line < llocp->first_line); c++) {
		if ('\n' == *c) {
			cur_line++;
			offset = -1;
		}
	}
	issue_line = c;
	/* find the next newline or null terminator */
	for (; ('\0' != *c) && ('\n' != *c); c++) {
		// Left blank
	}
	/* store the old line terminator as we will need to restore it at the end
	 * otherwise other commands on the same line will not run properly
	 */
	old_terminator = *c;
	line_end = c;

	// Print this line
	*line_end = '\0';
	fprintf(err_buffer, "\n%s\n", line_begin);

	// Underline the issue
	c = issue_line;
	/* offset is the length of previous queries on the same line */
	for (; ('\0' != *c) && (cur_column < offset + llocp->first_column); c++, cur_column++) {
		'\t' == *c ? fprintf(err_buffer, "\t") : fprintf(err_buffer, " ");
	}
	for (; ('\0' != *c) && (cur_column < offset + llocp->last_column); c++, cur_column++) {
		fprintf(err_buffer, "^");
	}
	fprintf(err_buffer, "\n");
	/* restore terminator */
	*line_end = old_terminator;
}
