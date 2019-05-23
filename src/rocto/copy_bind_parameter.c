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

char *copy_text_parameter(RoctoSession *session, Bind *bind, const int cur_parm, char *query, const char *end_query) {
	char *text_parm_start, *text_parm_end;
	ErrorResponse *err;

	// Copy text value
	text_parm_start = (char*)bind->parms[cur_parm].value;
	text_parm_end = text_parm_start + bind->parms[cur_parm].length;
	while(text_parm_start < text_parm_end) {
		*query = *text_parm_start;
		query++;
		text_parm_start++;
		// We need to leave an extra place for the closing quote, hence
		//  the +1
		if(query + 1 >= end_query) {
			err = make_error_response(PSQL_Error_ERROR,
						  PSQL_Code_Syntax_Error,
						  "expression exceeds maximum buffer length",
						  0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
			return NULL;
		}
	}
	return query;
}

char *copy_binary_parameter(RoctoSession *session, Bind *bind, const int cur_parm, char *query, const char *end_query) {
	char *binary_parm_start, *binary_parm_end;
	char str_value[MAX_STR_CONST];
	int64_t value = 0;
	int copied = 0;
	char uuid[MAX_STR_CONST];
	ErrorResponse *err;

	switch (bind->parms[cur_parm].length) {
		case 1:
			value = bin_to_char(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%ld", value);
			break;
		case 2:
			value = bin_to_int16(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%ld", value);
			break;
		case 4:
			value = bin_to_int32(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%ld", value);
			break;
		case 8:
			// This covers the OID case, as it is just an integer
			value = bin_to_int64(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%ld", value);
			break;
		case 36:
			bin_to_uuid(bind->parms[cur_parm].value, uuid, MAX_STR_CONST);
			copied = snprintf(str_value, MAX_STR_CONST, "%s", uuid);
			break;
		default:
			err = make_error_response(PSQL_Error_ERROR,
						  PSQL_Code_Syntax_Error,
						  "unsupported bind parameter type received",
						  0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
			return NULL;
	}
	if (0 > copied || query + copied > end_query) {
			err = make_error_response(PSQL_Error_ERROR,
						  PSQL_Code_Syntax_Error,
						  "failed to decode binary bind parameter",
						  0);
			send_message(session, (BaseMessage*)(&err->type));
			free_error_response(err);
			return NULL;
	}
	strncpy(query, str_value, copied);
	query += copied;
	return query;
}
