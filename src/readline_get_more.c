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

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "octo.h"

static boolean_t query_cancelled; /* TRUE if a Ctrl-C discarded lines already read for the current query */

/* readline invokes this hook when a signal interrupts its wait for input, after it has invoked the application's handler
 * for that signal ("ctrlc_handler()" in "octo.c" for a SIGINT, which sets "ctrlc_pressed"). readline catches the SIGINT
 * first and switches the terminal out of the mode it reads in before it invokes that handler. On a Ctrl-C, discard the
 * line being edited and display a fresh "OCTO>" prompt, as psql does.
 * readline keeps waiting for input after this; it has no way to return from "readline()" without one.
 */
static int ctrlc_event_hook(void) {
	if (ctrlc_pressed) {
		ctrlc_pressed = FALSE;
		/* If lines of the current query were already read, the parser has consumed them. They cannot be discarded
		 * until "readline()" returns the next line, so note that here.
		 */
		if (old_input_index < cur_input_index) {
			query_cancelled = TRUE;
		}
		rl_replace_line("", 0);
		rl_crlf();
		rl_on_new_line();
		rl_redisplay();
	}
	return 0;
}

int readline_get_more(void) {
	int   line_length, data_read;
	char *line;
	if (config->is_tty) {
		struct sigaction alrm_ydb, alrm_restart;

		/* While in "readline()", the handler readline installs for the signals it catches records only the most
		 * recent one, and readline acts on that once it gets control back. So a SIGALRM from a YottaDB timer that
		 * arrives along with another signal readline catches (for example a SIGTSTP or SIGINT from the terminal, or
		 * a SIGTERM) overwrites it and that signal is lost. readline does not take over SIGALRM if the handler
		 * already in place has SA_RESTART set, so set that flag on the YottaDB handler for the duration of the call.
		 * That handler then runs directly, as it does outside "readline()".
		 */
		sigaction(SIGALRM, NULL, &alrm_ydb);
		alrm_restart = alrm_ydb;
		alrm_restart.sa_flags |= SA_RESTART;
		sigaction(SIGALRM, &alrm_restart, NULL);
		/* A Ctrl-C that arrived since the last "readline()" call (for example after a query finished) had nothing
		 * to cancel. Do not act on it at this prompt.
		 */
		ctrlc_pressed = FALSE;
		rl_signal_event_hook = ctrlc_event_hook;
		line = readline("OCTO> ");
		sigaction(SIGALRM, &alrm_ydb, NULL);
		/* It is possible a signal (for example a SIGTERM) whose handling YottaDB deferred arrived while inside the
		 * "readline()" call above. Take this opportunity to handle it.
		 */
		ydb_eintr_handler();
		if (query_cancelled) {
			/* Discard the lines already read for the current query and place this line where they began.
			 * Have the lexer see the end of input so the parser stops, without an error, and "octo.c" then
			 * parses this line as the start of a new query.
			 */
			query_cancelled = FALSE;
			assert(EOF_NONE == eof_hit);
			assert(old_input_index < cur_input_index);
			cur_input_index = old_input_index;
			input_buffer_combined[cur_input_index] = '\0';
			eof_hit = ((NULL == line) ? EOF_CANCEL_EXIT : EOF_CANCEL);
		}
		if (NULL == line) {
			// Detecting the EOF is handled by the lexer and this should never be true at this stage
			assert((EOF_NONE == eof_hit) || (EOF_CANCEL_EXIT == eof_hit));
			return 0;
		}
		line_length = strlen(line);
		// Trim the trailing white space here so that cur_input_index is always at the end of a query
		// Otherwise the buffer will not be reset and multiple queries will end up in the debug info
		int is_white_space = TRUE;
		while (is_white_space && (0 < line_length)) {
			switch (line[line_length - 1]) {
			case ' ':
				line_length--;
				break;
			case '\t':
				line_length--;
				break;
			default:
				line[line_length] = '\0';
				is_white_space = FALSE;
				break;
			}
		}
		if (0 == line_length) {
			/* This means a user hit enter
			 * there is nothing to do
			 */
			free(line);
			return 1;
		}
		COPY_QUERY_TO_INPUT_BUFFER(line, line_length, NEWLINE_NEEDED_TRUE); /* will resize as needed */
		free(line);
		return line_length;
	} else {
		/* if query spans the entire buffer then our query is larger than the current buffer
		 * so double it (plus 1 for \0) and read in to the new space
		 */
		do {
			// Reset errno to ensure that the loop terminates so long as `EINTR` is not raised by `read()`
			if (cur_input_index != cur_input_max) {
				assert(cur_input_max > cur_input_index);
				data_read = read(fileno(inputFile), input_buffer_combined + cur_input_index,
						 cur_input_max - cur_input_index);
			} else if ((old_input_index * 2) > cur_input_max) {
				memmove(input_buffer_combined, input_buffer_combined + old_input_index,
					cur_input_max - old_input_index);
				cur_input_index -= old_input_index;
				old_input_index = 0;
				/* Note: old_input_line_num does not need to be updated as its current value is valid */
				old_input_line_begin = input_buffer_combined;
				data_read = read(fileno(inputFile), input_buffer_combined + cur_input_index,
						 cur_input_max - cur_input_index);
			} else {
				char  *tmp;
				size_t old_begin_index;

				assert(old_input_line_begin >= input_buffer_combined);
				old_begin_index = old_input_line_begin - input_buffer_combined;
				tmp = malloc(cur_input_max * 2 + 1);
				memcpy(tmp, input_buffer_combined, cur_input_max);
				free(input_buffer_combined);
				input_buffer_combined = tmp;
				old_input_line_begin = &input_buffer_combined[old_begin_index];
				data_read = read(fileno(inputFile), input_buffer_combined + cur_input_max, cur_input_max);
				cur_input_max *= 2;
			}
		} while ((-1 == data_read) && (EINTR == errno));

		// Detecting the EOF is handled by the lexer and this should never be true at this stage
		assert(EOF_NONE == eof_hit);
		if (data_read == -1) {
			ERROR(ERR_SYSCALL, "read", errno, strerror(errno));
			return 0;
		}
		input_buffer_combined[cur_input_index + data_read] = '\0';
		return data_read;
	}
}
