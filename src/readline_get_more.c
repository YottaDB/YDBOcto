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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "octo.h"
#include "octo_types.h"

#define YY_NULL 0

int get_input(char *buf, int size) {
	UNUSED(size);
	if(eof_hit)
		return YY_NULL;
	if(input_buffer_combined[cur_input_index] == '\0') {
		if(cur_input_more() == 0) {
			return YY_NULL;
		}
		return -1;
	}
	buf[0] = input_buffer_combined[cur_input_index++];
	return 1;
}

int readline_get_more() {
	int line_length, data_read;
	char *line;
	if(config->is_tty) {
		line = readline("OCTO> ");
		if(line == NULL) {
			// Detecting the EOF is handled by the lexer and this should never be true at this stage
			assert(FALSE == eof_hit);
			return 0;
		}
		line_length = strlen(line);
		// Trim the trailing white space here so that cur_input_index is always at the end of a query
		// Otherwise the buffer will not be reset and multiple queries will end up in the debug info
		int is_white_space = TRUE;
		while(is_white_space){
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
		if(line_length == 0) {
			/* This means a user hit enter
			 * there is nothing to do
			 */
			return 1;
		}
		/* if input line is too long resize buffer
		 * by min(cur_input_max * 2, line_length) + 2 (for the \n\0)
		 */
		if(line_length >= cur_input_max - cur_input_index - 2) {
			int resize_amt = line_length > (cur_input_max * 2) ? line_length : (cur_input_max * 2);
			char *tmp = malloc(resize_amt + 2);
			memcpy(tmp, input_buffer_combined, cur_input_index);
			free(input_buffer_combined);
			input_buffer_combined = tmp;
			cur_input_max = resize_amt;
		}
		memcpy(&input_buffer_combined[cur_input_index], line, line_length);
		input_buffer_combined[cur_input_index + line_length] = '\n';
		input_buffer_combined[cur_input_index + line_length+1] = '\0';
		free(line);
		return line_length;
	} else {
		/* if query spans the entire buffer then our query is larger than the current buffer
		 * so double it (plus 1 for \0) and read in to the new space
		 */
		if (old_input_index == 0 && cur_input_index == cur_input_max) {
			char *tmp = malloc(cur_input_max * 2 + 1);
			memmove(tmp, input_buffer_combined, cur_input_max);
			free(input_buffer_combined);
			input_buffer_combined = tmp;
			data_read = read(fileno(inputFile), input_buffer_combined + cur_input_max, cur_input_max);
			cur_input_max *= 2;
		/* if just the cur_input_index is the max then we probably have a dangling query
		 * copy everything from the old index to the end to the start of the buffer
		 * shift the index over and read more
		 */
		} else if(cur_input_index == cur_input_max) {
			memcpy(input_buffer_combined, input_buffer_combined + old_input_index, cur_input_index - old_input_index);
			cur_input_index -= old_input_index;
			old_input_index = 0;
			data_read = read(fileno(inputFile), input_buffer_combined + cur_input_index, cur_input_max - cur_input_index);
		} else {
			data_read = read(fileno(inputFile), input_buffer_combined, cur_input_max);
		}

		// Detecting the EOF is handled by the lexer and this should never be true at this stage
		assert(FALSE == eof_hit);
		if(data_read == -1) {
			ERROR(ERR_SYSCALL, "read", errno, strerror(errno));
			return 0;
		}
		input_buffer_combined[cur_input_index + data_read] = '\0';
		return data_read;
	}
}
