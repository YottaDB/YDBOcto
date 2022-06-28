/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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
SqlStatement *parse_line(ParseContext *parse_context) {
	SqlStatement *result = 0;
	yyscan_t      scanner;

	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&lex_buffer, OCTO_INIT_BUFFER_LEN);
	lex_buffer.buf_addr[0] = '\0';
	lex_buffer.len_used = 0;
	if (yylex_init(&scanner)) {
		YDB_FREE_BUFFER(&lex_buffer);
		ERROR(ERR_INIT_SCANNER, "");
		return NULL;
	}
	config->plan_id = 1; /* Start valid unique_id for tables at 1 (relied upon by "hash_canonical_query"
			      * when referencing "tbl_and_col_id" field in SqlColumnListAlias)
			      */
	yyparse(scanner, &result, &config->plan_id, parse_context);
	yylex_destroy(scanner);
	YDB_FREE_BUFFER(&lex_buffer);
	/* Remove newline at end of query line if present.
	 * It will be present for octo but not necessarily for rocto in case query comes in from a client.
	 */
	if ((0 < cur_input_index) && ('\n' == input_buffer_combined[cur_input_index - 1]))
		cur_input_index--;
	return result;
}
