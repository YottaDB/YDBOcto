/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#include "parser.h"

void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, char const *s)
{
	printf("Error with syntax near (line %d, column %d):", llocp->first_line + 1, llocp->first_column);
	print_yyloc(llocp);
	printf("%s\n", s);
}

void print_yyloc(YYLTYPE *llocp) {
	// llocp is 0 based
	int cur_line = 0, cur_column = 1;
	char *c = input_buffer_combined, *line_begin, *line_end;
	for(;*c != '\0' && cur_line < llocp->first_line; c++) {
		if(*c == '\n')
			cur_line++;
	}
	line_begin = c;
	for(; *c != '\0' && *c != '\n'; c++) {
		// Left blank
	}
	line_end = c;

	// Print this line
	*line_end = '\0';
	printf("\n%s\n", line_begin);

	// Underline the issue
	c = line_begin;
	for(; *c != '\0' && cur_column < llocp->first_column; c++, cur_column++) {
		printf(" ");
	}
	// Note that sometimes the first_column includes the space preceeding the first
	//  bad text; if that is the case, skip it
	if(*c == ' ') {
		printf(" ");
	}
	for(; *c != '\0' && cur_column <= llocp->last_column; c++, cur_column++) {
		printf("^");
	}
	printf("\n");
}
