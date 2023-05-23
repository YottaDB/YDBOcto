/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

int no_more(void) { return 0; }

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
			readline_setup();
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
			ydb_long_t	 cursorId;
			ydb_buffer_t	 schema_global;
			SqlStatementType result_type = invalid_STATEMENT; // for History statement

			/* All current queries in the buffer will have been read when
			 * cur_input_index+1 is the location of \0 in the buffer.
			 * After this reset the buffer.
			 */
			if ('\0' == input_buffer_combined[cur_input_index + 1]) {
				cur_input_index = 0;
				/* `leading_spaces` must be reset here to prevent off-by-one issues with syntax highlighting when
				 * multi-query lines are submitted in succession. For more information, see the discussion thread at
				 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1237#note_1216819522.
				 */
				leading_spaces = 0;
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

			/* Grab result type (set to invalid_STATEMENT originally) before OCTO_CFREE,
			 * which will discard the result variable.
			 */
			if (NULL != result)
				result_type = result->type;

			DELETE_QUERY_PARAMETER_CURSOR_LVN(&cursor_ydb_buff);
			OCTO_CFREE(memory_chunks);
			save_eof_hit = eof_hit; /* Save a copy of the global "eof_hit" in a local variable */
			/* else: INFO_PARSING_DONE message will be invoked inside "run_query()" call later below */
			if (EOF_NONE != eof_hit) {
				/* If Octo was started without an input file (i.e. sitting at the "OCTO>" prompt) and
				 * Ctrl-D was pressed by the user, then print a newline to cleanly terminate the current line
				 * before exiting. No need to do this in case EXIT or QUIT commands were used as we will not
				 * be sitting at the "OCTO>" prompt in that case.
				 */
				if (EOF_CTRLD == eof_hit) {
					printf("\n");
				}

				/* The purpose of this block is to add ";" so that a previous
				 * query, prior to CTRL-D, will be processed, when you type
				 * query w/o ";", and then CTRL-D. However, QUIT and EXIT also
				 * come here, for no good reason. We just need to handle
				 * everything appropriately.
				 *
				 * Note that 'select * from names quit' won't be parsed, so this
				 * block is really only for CTRL-D.
				 *
				 * We add semicolon, newline, and increment cur_input_index
				 */
				assert(cur_input_index < cur_input_max);
				if ((0 < cur_input_index) && ('\n' == input_buffer_combined[cur_input_index])) {
					// 'QUIT;' is legal, and we don't want to add another ;
					if (';' != input_buffer_combined[cur_input_index - 1]) {
						input_buffer_combined[cur_input_index] = ';';
						if ((cur_input_index + 1) < cur_input_max) {
							input_buffer_combined[++cur_input_index] = '\n';
							if ((cur_input_index + 1) < cur_input_max) {
								/* It is possible "input_buffer_combined" contains
								 * non-null content from previous queries at "cur_input_index"
								 * due to adding '\n' at the end (which would have overwritten
								 * a '\0' at the end). Therefore add the '\0' back as otherwise
								 * one would see some prior query content in error messages
								 * that could be confusing (YDBOcto#936).
								 */
								input_buffer_combined[cur_input_index + 1] = '\0';
							}
						}
					}
				}

				/* This block only also runs with CTRL-D, but only when pressed
				 * on a blank line and no other queries were previously entered.
				 * It has the effect of terminating Octo.
				 */
				if (old_input_index == cur_input_index) {
					break;
				}

				/* reset global to avoid "get_input()" (called from "run_query()"
				 * below) from prematurely returning YY_NULL.
				 */
				eof_hit = EOF_NONE;
			}

			/* Before checking result of "parse_line()" call, add the current
			 * input line to the readline history.
			 * We replace the last character with a null terminator prior to
			 * adding history, then restore it.
			 */
			placeholder = input_buffer_combined[cur_input_index];
			input_buffer_combined[cur_input_index] = '\0';
			add_single_history_item(input_buffer_combined, old_input_index);
			input_buffer_combined[cur_input_index] = placeholder;

			/* Now that readline history addition is done, get back to checking return value from "parse_line()" */
			if (NULL == result) {
				INFO(INFO_PARSING_DONE, cur_input_index - old_input_index, input_buffer_combined + old_input_index);
				INFO(INFO_RETURNING_FAILURE, "octo()");
				continue;
			}

			/* History statement (\s) does not need to be processed further */
			/* NB: Switch statement here for future no-op statements like the
			 * history statement.
			 */
			if (invalid_STATEMENT != result_type) {
				switch (result_type) {
				case history_STATEMENT:
					continue;
				default:
					break;
				}
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

	// Save readline history for interactive sessions
	if (config->is_tty) {
		save_readline_history();
	}

	cleanup_tables();
	CLEANUP_CONFIG(config->config_file);
	return ret;
}
