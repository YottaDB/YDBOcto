/****************************************************************
 *								*
 * Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Executes a sequence of SQL queries stored in file name "query_file_name" */
int run_query_file(char *query_file_name) {
	FILE *save_inputFile;
	int (*save_cur_input_more)(void);
	int status;

	/* Open the seed file to parse/run queries and set the global variable "inputFile" to point to that. */
	save_inputFile = inputFile; /* Save "inputFile" (non-NULL value possible in case of "octo -f" invocation */
	inputFile = fopen(query_file_name, "r");
	if (NULL == inputFile) {
		ERROR(ERR_FILE_NOT_FOUND, query_file_name);
		assert(FALSE);
		return 1;
	}
	/* Change global variables to reflect that we are now going to read queries from "octo-seed.sql" */
	/* Set "readline_get_more()" as the function to read/parse query lines from "inputFile".
	 * But before that, save current value of "cur_input_more" in temporary variable.
	 */
	save_cur_input_more = cur_input_more;
	cur_input_more = &readline_get_more;
	/* Ready input buffer for reading from seed file */
	cur_input_index = 0;
	cur_input_line_num = 0;
	input_buffer_combined[cur_input_index] = '\0';
	/* Read query lines from "inputFile" until end */
	do {
		ParseContext parse_context;

		memset(&parse_context, 0, sizeof(parse_context));
		/* Caller should have set this to TRUE. Needed by "run_query()" to bypass
		 * ERR_CANNOT_CREATE_TABLE/ERR_CANNOT_CREATE_FUNCTION errors.
		 */
		assert(config->in_auto_load_octo_seed || config->is_auto_upgrade_octo929);
		status = run_query(&print_temporary_table, NULL, PSQL_Invalid, &parse_context);
		if (0 != status) {
			break;
		}
		if (EOF_NONE != eof_hit) {
			break;
		}
	} while (!feof(inputFile));
	fclose(inputFile);
	/* Restore global variables now that seed file loading is done */
	cur_input_more = save_cur_input_more;
	inputFile = save_inputFile;
	/* Reset query processing related global variables that might have been modified in above "run_query()" loop */
	eof_hit = EOF_NONE;
	ERASE_INPUT_BUFFER; /* Clear history of all "parse_line()" processing related to binary function upgrade to avoid
			     * confusion when we next proceed to run real queries.
			     */
	return status;
}
