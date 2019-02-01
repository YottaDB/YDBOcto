/* Copyright (C) 2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "octo.h"
#include "octo_types.h"

#define YY_NULL 0

int get_input(char *buf, int size) {
	//printf("current left: %s\n", &input_buffer_combined[cur_input_index]);
	if(cur_input_index == cur_input_max
	   || input_buffer_combined[cur_input_index] == '\0') {
		cur_input_index = 0;
		//printf("Looking for more input...\n");
		if(cur_input_more() == 0)
			return YY_NULL;
		return get_input(buf, size);
	}
	buf[0] = input_buffer_combined[cur_input_index++];
	//buf[1] = '\0';
	//printf("%c", buf[0]);
	return 1;
}

int readline_get_more() {
	int line_length;
	char *line, c;
	if(isatty(fileno(inputFile))) {
		line = readline("OCTO> ");
		if(line == NULL)
			return 0;
		line_length = strlen(line);
		if(line_length == 0) {
			// This means a user hit enter
			input_buffer_combined[0] = '\n';
			input_buffer_combined[1] = '\0';
			return 1;
		}
		add_history(line);
		if(line_length >= cur_input_max - 2) {
			ERROR(ERR_LINE_TOO_LONG);
			return 0;
		}
		memcpy(input_buffer_combined, line, line_length);
		input_buffer_combined[line_length] = '\n';
		input_buffer_combined[line_length+1] = '\0';
		free(line);
		return line_length;
	} else {
		if(feof(inputFile))
			return 0;
		c = fgetc(inputFile);
		if(c == -1)
			return 0;
		input_buffer_combined[0] = c;
		input_buffer_combined[1] = '\0';
		return 1;
	}
}
