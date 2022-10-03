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
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "octo.h"

int readline_get_more() {
	int   line_length, data_read;
	char *line;
	if (config->is_tty) {
		line = readline("OCTO> ");
		/* It is possible the user pressed a Ctrl-C while inside the "readline()" call above.
		 * In that case, we need to handle the signal in a timely fashion. Take this opportunity to do that.
		 * If a Ctrl-C was indeed pressed, we will halt right away just like the user wants.
		 */
		ydb_eintr_handler();
		if (NULL == line) {
			// Detecting the EOF is handled by the lexer and this should never be true at this stage
			assert(EOF_NONE == eof_hit);
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
			if (cur_input_index == cur_input_max) {
				char * tmp;
				size_t old_begin_index;

				assert(old_input_line_begin >= input_buffer_combined);
				old_begin_index = old_input_line_begin - input_buffer_combined;
				tmp = malloc(cur_input_max * 2 + 1);
				memmove(tmp, input_buffer_combined, cur_input_max);
				free(input_buffer_combined);

				input_buffer_combined = tmp;
				old_input_line_begin = &input_buffer_combined[old_begin_index];
				data_read = read(fileno(inputFile), input_buffer_combined + cur_input_max, cur_input_max);
				cur_input_max *= 2;
			} else {
				assert(cur_input_max > cur_input_index);
				data_read = read(fileno(inputFile), input_buffer_combined + cur_input_index,
						 cur_input_max - cur_input_index);
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
