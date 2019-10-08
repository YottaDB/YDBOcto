/****************************************************************
 *								*
 * Copyright (c) 2019,2020 YottaDB LLC and/or its subsidiaries.	*
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

/* if rocto is running the error needs to be replicated to
 * be printed via octo_log which replicated to rocto and psql logs
 * otherwise print via the err_buffer
 */
#define PRINT_YYERROR(...) 							\
	if (config->is_rocto) {							\
		ERROR(CUSTOM_ERROR, ##__VA_ARGS__);				\
	} else {								\
		fprintf(err_buffer, ##__VA_ARGS__);				\
		fprintf(err_buffer, "\n"); /* formatting */					\
	}

#define SNPRINTF_ERR_BUFF(...)										\
	do {												\
		written = snprintf(err_ptr, err_out_len - (err_ptr - err_out), ##__VA_ARGS__);	\
		if (written < (err_out_len - (err_ptr - err_out))) {					\
			err_ptr += written;								\
			break;										\
		} else {										\
			err_out_len *= 2;								\
			char *tmp = malloc(err_out_len);						\
			memcpy(tmp, err_out, err_ptr - err_out);					\
			err_ptr = tmp + (err_ptr - err_out);						\
			free(err_out);									\
			err_out = tmp;									\
		}											\
	} while (TRUE);

void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, char *cursorId, ParseContext *parse_context, char const *s)
{
	UNUSED(plan_id);
	UNUSED(cursorId);
	UNUSED(parse_context);
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
		switch(stmt->type) {
		case set_operation_STATEMENT:
		case table_alias_STATEMENT:
			/* In this case, use the location of the first select column_list operand */
			if (set_operation_STATEMENT == stmt->type) {
				UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
				sql_stmt = drill_to_table_alias(set_operation->operand[0]);
			} else {
				sql_stmt = stmt;
			}
			UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
			assert(NULL != table_alias->column_list);
			UNPACK_SQL_STATEMENT(cur_cla, table_alias->column_list, column_list_alias);
			llocp = &cur_cla->column_list->loc;
			break;
		default:
			llocp = &stmt->loc;
			break;
		}
	}
	if (llocp->first_line || llocp->first_column || llocp->last_column) {
		if (0 == llocp->first_line) {
			PRINT_YYERROR("Error with syntax near (line %d, column %d):", llocp->first_line + 1,		\
										llocp->first_column + leading_spaces);
		} else {
			PRINT_YYERROR("Error with syntax near (line %d, column %d):", llocp->first_line + 1, llocp->first_column);
		}
		print_yyloc(llocp);
		if (NULL != s) {
			PRINT_YYERROR("%s\n", s);
		}
	} else {
		assert(NULL == s);
	}
}

void print_yyloc(YYLTYPE *llocp) {
	// llocp is 0 based
	int cur_line = 0, cur_column = 0, offset = old_input_index, err_out_len = MAX_STR_CONST, written;
	char *c, *line_begin = input_buffer_combined, *line_end, *issue_line, old_terminator;
	char *err_out, *err_ptr;

	err_out = malloc(err_out_len);
	err_ptr = err_out;

	/* start at beginning of buffer
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
	/* because files are read as one continous input extra newlines are not trimed by the parser
	 * so count them and adjust the underline offset
	 */
	if (!config->is_tty) {
		c--;
		for (; (0 != cur_column) && ('\n' == *(c - 1)); c--, cur_column--)
			offset++;
	}
	cur_column = 0;

	/* for formatting purposes only print this if we are not in rocto */
	if (!config->is_rocto)
		fprintf(err_buffer, "\n");
	/* if a newline is found in this loop then offset is -1 (-1 not 0 due to spacing)
	 * because no previous query could be on that line
	 * additionally print the current line
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
	PRINT_YYERROR("%s", line_begin);

	// Underline the issue
	c = issue_line;
	/* offset is the length of previous queries on the same line */
	for (; ('\0' != *c) && (cur_column < offset + llocp->first_column); c++, cur_column++) {

		if ('\t' == *c) {
		       	SNPRINTF_ERR_BUFF("\t");
		} else {
			SNPRINTF_ERR_BUFF(" ");
		}
	}
	for (; ('\0' != *c) && (cur_column < offset + llocp->last_column); c++, cur_column++) {
		SNPRINTF_ERR_BUFF("^");
	}
	PRINT_YYERROR("%s", err_out);
	free(err_out);
	/* restore terminator */
	*line_end = old_terminator;
}
