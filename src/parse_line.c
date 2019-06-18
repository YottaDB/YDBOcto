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
#include "parser.h"
#include "lexer.h"

/**
 * Parses line, which should end with a semicolon, and returns the result
 *
 * NOTE: caller is responsible for freeing the return value
 *
 * @returns the parsed statement, or NULL if there was an error parsing.
 */
SqlStatement *parse_line(const char *line) {
	SqlStatement *result = 0;
	yyscan_t scanner;
	int line_length;

	if(line != input_buffer_combined) {
		INFO(CUSTOM_ERROR, "Moving line to input_buffer_combined");
		line_length = strlen(line);
		if(line_length >= MAX_STR_CONST - 1) {
			ERROR(ERR_LINE_TOO_LONG, "");
			return NULL;
		}
		strncpy(input_buffer_combined, line, line_length);
		input_buffer_combined[line_length] = '\0';
		cur_input_index = 0;
	}

	if (yylex_init(&scanner))
		FATAL(ERR_INIT_SCANNER, "");

	config->plan_id = 0;
	int status = yyparse(scanner, &result, &config->plan_id);
	yylex_destroy(scanner);
	if(status)
	{
		ERROR(ERR_PARSING_COMMAND, input_buffer_combined);
		return NULL;
	}

	// For some reason, the lexer reads one past the end; decrement that
	if(cur_input_index > 0) {
		cur_input_index--;
	}
	return result;
}
