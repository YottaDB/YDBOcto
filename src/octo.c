/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#include <readline/history.h>

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "lexer.h"

extern int yydebug;

int no_more() { return 0; }

int main(int argc, char **argv) {
	ParseContext parse_context;
	int	     status, ret = YDB_OK;

	inputFile = NULL;
	status = octo_init(argc, argv);
	if (0 != status) {
		return status;
	}

	TRACE(INFO_OCTO_STARTED, "");
	yydebug = (TRACE == config->verbosity_level); /* Enable yacc/flex/bison tracing if verbosity was set to TRACE */
	cur_input_more = &readline_get_more;
	if (NULL == inputFile) {
		inputFile = stdin;
		/* Check if stdin is a terminal. If so, we need to use "readline()" for command line editing. */
		if (isatty(0)) {
			config->is_tty = TRUE;
		}
	}
	cur_input_index = 0;
	input_buffer_combined[cur_input_index] = '\0';
	do {
		ydb_buffer_t  cursor_ydb_buff;
		char	      cursor_buffer[INT64_TO_STRING_MAX];
		char	      placeholder;
		SqlStatement *result;
		int	      save_eof_hit;

		if (config->is_tty) { /* Clear previously read query from input buffer before starting to read new query.
				       * This lets octo -vv dump the current query that is being parsed instead of dumping
				       * all queries that have been keyed in till now.
				       */
			HIST_ENTRY * cur_hist;
			ydb_long_t   cursorId;
			ydb_buffer_t schema_global;

			/* All current queries in the buffer will have been read when
			 * cur_input_index+1 is the location of \0 in the buffer.
			 * After this reset the buffer.
			 */
			if ('\0' == input_buffer_combined[cur_input_index + 1]) {
				cur_input_index = 0;
				input_buffer_combined[cur_input_index] = '\0';
			}
			memset(&parse_context, 0, sizeof(parse_context));
			cursor_ydb_buff.buf_addr = cursor_buffer;
			cursor_ydb_buff.len_alloc = sizeof(cursor_buffer);
			YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
			cursorId = create_cursor(&schema_global, &cursor_ydb_buff);
			if (0 > cursorId) {
				break; /* Exit from "OCTO>" prompt in case of errors in "create_cursor()" */
			}
			parse_context.cursorId = cursorId;
			parse_context.cursorIdString = cursor_ydb_buff.buf_addr;
			memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
			old_input_index = cur_input_index;
			/* Parse query first BEFORE going into "run_query()" (which requires a read-only lock).
			 * This way we avoid posing problems for any concurrent DDL operations that require a read-write lock
			 * particularly in case we have a multi-line query and are waiting for user input.
			 * If the "parse_line()" call succeeds below, we will invoke "parse_line()" again later inside
			 * "run_query()" with the already parsed (and potentially multi-line) query. This is achieved by
			 * resetting "cur_input_index" to "old_input_index" a few lines below.
			 */
			result = parse_line(&parse_context);
			DELETE_QUERY_PARAMETER_CURSOR_LVN(&cursor_ydb_buff);
			OCTO_CFREE(memory_chunks);
			/* Before checking result of "parse_line()" call, add the current input line to the readline history */
			save_eof_hit = eof_hit; /* Save a copy of the global "eof_hit" in a local variable */
			/* else: INFO_PARSING_DONE message will be invoked inside "run_query()" call later below */
			/* Add the current query to the readlines history */
			if (EOF_NONE != eof_hit) {
				/* If Octo was started without an input file (i.e. sitting at the "OCTO>" prompt) and
				 * Ctrl-D was pressed by the user, then print a newline to cleanly terminate the current line
				 * before exiting. No need to do this in case EXIT or QUIT commands were used as we will not
				 * be sitting at the "OCTO>" prompt in that case.
				 */
				if (EOF_CTRLD == eof_hit) {
					printf("\n");
				}
				assert(cur_input_index < cur_input_max);
				if ((0 < cur_input_index) && ('\n' == input_buffer_combined[cur_input_index])) {
					input_buffer_combined[cur_input_index] = ';';
					if ((cur_input_index + 1) < cur_input_max) {
						input_buffer_combined[cur_input_index + 1] = '\n';
					}
				}
				if (old_input_index == cur_input_index) {
					break;
				}
				eof_hit = EOF_NONE; /* reset global to avoid "get_input()" (called from "run_query()"
						     * below) from prematurely returning YY_NULL.
						     */
			}
			placeholder = input_buffer_combined[cur_input_index];
			input_buffer_combined[cur_input_index] = '\0';
			/* get the last item added to the history
			 * if it is the same as the current query don't add it to the history again
			 */
			cur_hist = history_get(history_length);
			if (NULL != cur_hist) {
				if (0 != strcmp(cur_hist->line, input_buffer_combined + old_input_index))
					add_history(input_buffer_combined + old_input_index);
			} else {
				add_history(input_buffer_combined + old_input_index);
			}
			input_buffer_combined[cur_input_index] = placeholder;
			/* Now that readline history addition is done, get back to checking return value from "parse_line()" */
			if (NULL == result) {
				INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
				INFO(INFO_RETURNING_FAILURE, "octo()");
				continue;
			}
			cur_input_index = old_input_index; /* This ensures that the already parsed query is presented again
							    * to "parse_line()" invocation in "run_query()" call below.
							    */
		} else {
			save_eof_hit = FALSE; /* Needed to avoid a false -Wmaybe-uninitialized warning on "save_eof_hit" */
		}
		/* else: It is a file input and we cannot easily clear input buffer */
		// Read new query and run it at the same time and discard return value
		// Any meaningful errors will have already been reported lower in the stack and failed queries are recoverable,
		// so it can safely be discarded.
		memset(&parse_context, 0, sizeof(parse_context));
		status = run_query(&print_temporary_table, NULL, PSQL_Invalid, &parse_context);
		if (YDB_OK != status) {
			ret = status;
		}

		if (config->is_tty) {
			eof_hit = save_eof_hit; /* Restore global from saved local value now that "run_query()" is done */
		}
		if (EOF_NONE != eof_hit) {
			break;
		}
	} while (!feof(inputFile));
	cleanup_tables();
	CLEANUP_CONFIG(config->config_file);
	return ret;
}
