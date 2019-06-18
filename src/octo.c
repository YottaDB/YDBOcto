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
#include "physical_plan.h"
#include "parser.h"
#include "lexer.h"

extern int yydebug;

int no_more() {
	return 0;
}

int main(int argc, char **argv)
{
	int c, error = 0, status;
	int done;
	SqlValue *value;
	SqlTable *table, *t_table;
	SqlStatement *tmp_statement;

	inputFile = NULL;
	octo_init(argc, argv);

	TRACE(CUSTOM_ERROR, "Octo started");

	/* Load the existing tables */

	yydebug = config->record_error_level == TRACE;
	cur_input_more = &readline_get_more;
	if (inputFile == NULL) {
		inputFile = stdin;
		config->is_tty = TRUE;
	}
	cur_input_index = 0;
	memset(input_buffer_combined, 0, MAX_STR_CONST);
	do {
		if(run_query(input_buffer_combined, &print_temporary_table, NULL) == 0) {
		}
		if(eof_hit)
			break;
	} while(!feof(inputFile));

	cleanup_tables();
	return error;
}
