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
		// Otherwise the buffer will not be reset and multiple querys will end up in the debug info
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
		add_history(line);
		if(line_length >= cur_input_max - 2) {
			ERROR(ERR_LINE_TOO_LONG, "");
			return 0;
		}
		memcpy(&input_buffer_combined[cur_input_index], line, line_length);
		input_buffer_combined[cur_input_index + line_length] = '\n';
		input_buffer_combined[cur_input_index + line_length+1] = '\0';
		free(line);
		return line_length;
	} else {
		data_read = read(fileno(inputFile), input_buffer_combined, cur_input_max);
		/* only reset cur_input_index in the case of there being more data to read
		 * EOF (data_read == 0) should preserve the buffer for error reporting
		 */
		if (data_read > 0){
			cur_input_index = 0;
		}
		// Detecting the EOF is handled by the lexer and this should never be true at this stage
		assert(FALSE == eof_hit);
		if(data_read == -1) {
			FATAL(ERR_SYSCALL, "read", errno, strerror(errno));
		}
		input_buffer_combined[cur_input_index + data_read] = '\0';
		return data_read;
	}
}
