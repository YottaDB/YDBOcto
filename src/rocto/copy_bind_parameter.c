/* Copyright (C) 2018-2019 YottaDB, LLC
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
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

#define INT16_MAX_DIGITS 5
#define INT32_MAX_DIGITS 10
#define INT64_MAX_DIGITS 20

int copy_text_parameter(RoctoSession *session, Bind *bind, const int cur_parm, char *query_ptr, const char *end_query_ptr) {
	char *text_parm_start, *text_parm_end;
	ErrorResponse *err;

	// Copy text value
	text_parm_start = bind->parms[cur_parm].value;
	text_parm_end = text_parm_start + bind->parms[cur_parm].length;
	while(text_parm_start < text_parm_end) {
		*query_ptr++ = *text_parm_start++;
		// We need to leave an extra place for the closing quote, hence
		//  the +1
		if(query_ptr + 1 >= end_query_ptr) {
			err = make_error_response(PSQL_Error_ERROR,
						  PSQL_Code_Syntax_Error,
						  "expression exceeds maximum buffer length",
						  0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
			return -1;
		}
	}
	return 0;
}

int copy_binary_parameter(RoctoSession *session, Bind *bind, const int cur_parm, char *query_ptr, const char *end_query_ptr) {
	char *binary_parm_start, *binary_parm_end;
	ErrorResponse *err;

	switch (bind->parms[cur_parm].length) {
		case 1:
			// This could be a char or bool...
		case 2:
			int64_t value = 0;
			char str_value[INT16_MAX_DIGITS+1];	// count null
			value = bin_to_int16(bind->parms[cur_parm].value);
			snprintf(str_value, INT16_MAX_DIGITS, "%d", value);
			// memcpy(query_ptr, str_value, INT_16
		default:
			err = make_error_response(PSQL_Error_ERROR,
						  PSQL_Code_Syntax_Error,
						  "unsupported bind parameter type received",
						  0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
			return -1;

	}
	// Copy text value
	binary_parm_start = bind->parms[cur_parm].value;
	binary_parm_end = binary_parm_start + bind->parms[cur_parm].length;
	while(binary_parm_start < binary_parm_end) {
		*query_ptr++ = *binary_parm_start++;
		// We need to leave an extra place for the closing quote, hence
		//  the +1
		if(query_ptr + 1 >= end_query_ptr) {
			err = make_error_response(PSQL_Error_ERROR,
						  PSQL_Code_Syntax_Error,
						  "expression exceeds maximum buffer length",
						  0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
			return -1;
		}
	}
	return 0;
}
