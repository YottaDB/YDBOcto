/****************************************************************
 *								*
 * Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	*
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

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "lexer.h"
#include "gtmxc_types_wrapper.h"

extern int yydebug;

int no_more(void) { return 0; }

/* SIGINT handler for an interactive session. It notes the Ctrl-C in "ctrlc_pressed", which "readline_get_more()" (at the
 * "OCTO>" prompt) and "print_temporary_table()" (while printing rows) act on. If the M code of a query is running, it also
 * sends the SIGUSR2 that cancels it (as rocto does for a CancelRequest): "zintr^%ydboctoZinterrupt", the $ZINTERRUPT
 * handler, then unwinds the M code and sets "%ydboctoCancel" for "is_query_canceled()". A Ctrl-C at any other time (for
 * example during a CREATE TABLE) has no effect.
 */
static void ctrlc_handler(int sig) {
	int save_errno;

	UNUSED(sig);
	save_errno = errno;
	ctrlc_pressed = TRUE;
	if (query_running_in_m) {
		/* If the YottaDB version is r1.30, "_ydboctoZinterrupt.m" expects SIGUSR1 and not SIGUSR2 */
		kill(getpid(), ((131 > ydb_release_number) ? SIGUSR1 : SIGUSR2));
	}
	errno = save_errno;
}

int main(int argc, char **argv) {
	ParseContext parse_context;
	int	     status, ret = YDB_OK;
	int	     save_cur_input_line_num;

	inputFile = NULL;
	/* Before invoking "ydb_init()" (inside "octo_init()") set env var to ensure SIGUSR2 is treated the same as SIGUSR1.
	 * This is needed so SIGUSR1 creates ZSHOW dump files and SIGUSR2 cancels the running query (see "zintr" in
	 * "_ydboctoZinterrupt.m"). "ctrlc_handler()" sends the SIGUSR2 on a Ctrl-C.
	 */
	setenv("ydb_treat_sigusr2_like_sigusr1", "1", TRUE);
	status = octo_init(argc, argv);
	if (0 != status) {
		return status;
	}
	ydb_buffer_t z_interrupt, z_interrupt_handler;
	YDB_LITERAL_TO_BUFFER("$ZINTERRUPT", &z_interrupt);
	YDB_LITERAL_TO_BUFFER("DO zintr^%ydboctoZinterrupt", &z_interrupt_handler);
	status = ydb_set_s(&z_interrupt, 0, NULL, &z_interrupt_handler);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return status;
	}
	TRACE(INFO_OCTO_STARTED, "");
	yydebug = (TRACE == config->verbosity_level); /* Enable yacc/flex/bison tracing if verbosity was set to TRACE */
	cur_input_more = &readline_get_more;
	if (NULL == inputFile) {
		inputFile = stdin;
		/* Check if stdin is a terminal. If so, we need to use "readline()" for command line editing. */
		if (isatty(0)) {
			struct sigaction int_octo;

			config->is_tty = TRUE;
			readline_setup();
			/* The YottaDB SIGINT handler terminates the process. In an interactive session, a Ctrl-C should instead
			 * discard the query being entered or cancel the query that runs, as psql does, so replace that handler.
			 */
			memset(&int_octo, 0, sizeof(int_octo));
			sigemptyset(&int_octo.sa_mask);
			int_octo.sa_handler = ctrlc_handler;
			sigaction(SIGINT, &int_octo, NULL);
		}
	}
	/* Now that all auto-upgrade and octo-seed related loading has occurred inside "octo_init()", set up
	 * "config->octo_print_query" based on whether "-p" was specified. We don't want to do this during
	 * auto upgrade etc. because that would cause a lot of query output (loading "octo-seed.sql" etc.)
	 * that would clutter the output. We also don't want to do this query printing if "readline()"
	 * is used for command line editing as that will display the query as the user enters it anyways.
	 */
	config->octo_print_query = config->octo_print_flag_specified && !config->is_tty;
	cur_input_index = 0;
	cur_input_line_num = 0;
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
				cur_input_line_num = 0;
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
			old_input_line_num = cur_input_line_num;
			save_cur_input_line_num = cur_input_line_num;
			// Kill view's cache created for previous query
			INIT_VIEW_CACHE_FOR_CURRENT_QUERY(config->global_names.loadedschemas, status);
			if (YDB_OK != status) {
				YDB_ERROR_CHECK(status);
			}
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
			if (IS_EOF_CANCEL(eof_hit)) {
				/* A Ctrl-C discarded the lines entered for this query. "readline_get_more()" placed the line
				 * entered after the Ctrl-C (if any) at "old_input_index". Parse it as the start of a new query.
				 * The discarded query is not added to the history.
				 */
				cur_input_index = old_input_index;
				cur_input_line_num = save_cur_input_line_num;
				old_input_line_begin = &input_buffer_combined[old_input_index];
				if (EOF_CANCEL_EXIT == eof_hit) {
					/* Ctrl-D was pressed after the Ctrl-C. Terminate as a Ctrl-D on an empty line does. */
					SAFE_PRINTF(fprintf, stdout, FALSE, FALSE, "%s", "\n");
					break;
				}
				eof_hit = EOF_NONE;
				continue;
			}
			if (EOF_NONE != eof_hit) {
				/* If Octo was started without an input file (i.e. sitting at the "OCTO>" prompt) and
				 * Ctrl-D was pressed by the user, then print a newline to cleanly terminate the current line
				 * before exiting. No need to do this in case EXIT or QUIT commands were used as we will not
				 * be sitting at the "OCTO>" prompt in that case.
				 */
				if (EOF_CTRLD == eof_hit) {
					SAFE_PRINTF(fprintf, stdout, FALSE, FALSE, "%s", "\n");
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
			cur_input_line_num = save_cur_input_line_num;
		} else {
			save_eof_hit = FALSE; /* Needed to avoid a false -Wmaybe-uninitialized warning on "save_eof_hit" */
		}
		/* else: It is a file input and we cannot easily clear input buffer */
		// Read new query and run it at the same time and discard return value
		// Any meaningful errors will have already been reported lower in the stack and failed queries are recoverable,
		// so it can safely be discarded.
		memset(&parse_context, 0, sizeof(parse_context));
		ctrlc_pressed = FALSE; /* Act only on a Ctrl-C that arrives while this query runs */
		status = run_query(&print_temporary_table, NULL, PSQL_Invalid, &parse_context);
		if (YDB_OK != status) {
			/* A canceled query is a failed query. Do not use QUERY_CANCELED (a negative value) as the exit status. */
			ret = ((QUERY_CANCELED == status) ? 1 : status);
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
	if (NULL != config->date_format) {
		free((char *)config->date_format);
	}
	if (NULL != config->timestamp_format) {
		free((char *)config->timestamp_format);
	}

	return ret;
}
