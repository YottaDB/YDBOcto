/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "lexer.h"

extern int yydebug;

int no_more() {
	return 0;
}

int main(int argc, char **argv)
{
	ParseContext	parse_context;
	int		status;

	inputFile = NULL;
	status = octo_init(argc, argv);
	if (0 != status) {
		return status;
	}

	TRACE(CUSTOM_ERROR, "Octo started");

	/* Load the existing tables */

	yydebug = config->verbosity_level == TRACE;
	cur_input_more = &readline_get_more;
	if (inputFile == NULL) {
		inputFile = stdin;
		config->is_tty = TRUE;
	}
	cur_input_index = 0;
	memset(input_buffer_combined, 0, MAX_STR_CONST);
	do {
		if (config->is_tty)
		{	/* Clear previously read query from input buffer before starting to read new query.
			 * This lets octo -vv dump the current query that is being parsed instead of dumping
			 * all queries that have been keyed in till now.
			 */
			/* All current queries in the buffer will have been read when
			 * cur_input_index+1 is the location of \0 in the buffer.
			 * After this reset the buffer.
			 */
			if(input_buffer_combined[cur_input_index+1] == '\0'){
				cur_input_index = 0;
				input_buffer_combined[cur_input_index] = '\0';
			}
		}
		/* else: It is a file input and we cannot easily clear input buffer */
		// Read new query and run it at the same time and discard return value
		// Any meaningful errors will have already been reported lower in the stack and failed queries are recoverable,
		// so it can safely be discarded.
		memset(&parse_context, 0, sizeof(parse_context));
		run_query(&print_temporary_table, NULL, FALSE, &parse_context);
		if(eof_hit)
			break;
	} while(!feof(inputFile));

	cleanup_tables();
	return 0;
}
