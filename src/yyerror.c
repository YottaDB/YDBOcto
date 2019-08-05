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
	UNUSED(out);
	UNUSED(scan);
	fprintf(err_buffer, "Error with syntax near (line %d, column %d):", llocp->first_line + 1, llocp->first_column);
	print_yyloc(llocp);
	fprintf(err_buffer, "%s\n", s);
}

void print_yyloc(YYLTYPE *llocp) {
	// llocp is 0 based
	int cur_line = 0, cur_column = 1;
	char *c = input_buffer_combined, *line_begin, *line_end, old_terminator;
	for(;*c != '\0' && cur_line < llocp->first_line; c++) {
		if(*c == '\n')
			cur_line++;
	}
	line_begin = c;
	for(; *c != '\0' && *c != '\n'; c++) {
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
	c = line_begin;
	for(; *c != '\0' && cur_column < llocp->first_column; c++, cur_column++) {
		fprintf(err_buffer, " ");
	}
	// Note that sometimes the first_column includes the space preceeding the first
	//  bad text; if that is the case, skip it
	if(*c == ' ') {
		fprintf(err_buffer, " ");
	}
	for(; *c != '\0' && cur_column <= llocp->last_column; c++, cur_column++) {
		fprintf(err_buffer, "^");
	}
	fprintf(err_buffer, "\n");
	/* restore terminator */
	*line_end = old_terminator;
}
