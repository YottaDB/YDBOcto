/* Copyright (C) 2018 YottaDB, LLC
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

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "lexer.h"

/**
 * Parses line, which should end with a semicolon, and returns the resolt
 *
 * NOTE: caller is responsible for freeing the return value
 *
 * @returns the parsed statement, or NULL if there was an error parsing.
 */
SqlStatement *parse_line(const char *line) {
	SqlStatement *result = 0;
	yyscan_t scanner;
	YY_BUFFER_STATE state;
	int line_length;

	if(line != input_buffer_combined) {
		line_length = strlen(line);
		if(line_length >= MAX_STR_CONST - 1) {
			ERROR(ERR_LINE_TOO_LONG);
			return NULL;
		}
		strncpy(input_buffer_combined, line, line_length);
		input_buffer_combined[line_length] = '\0';
		cur_input_index = 0;
	}

	if (yylex_init(&scanner))
		FATAL(ERR_INIT_SCANNER);

	//state = yy_scan_string(line, scanner);
	if(yyparse(scanner, &result, &config->plan_id))
	{
		ERROR(ERR_PARSING_COMMAND, input_buffer_combined);
		return NULL;
	}
	//yy_delete_buffer(state, scanner);
	yylex_destroy(scanner);
	return result;
}
