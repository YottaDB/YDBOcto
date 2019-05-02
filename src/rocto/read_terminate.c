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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

Terminate *read_terminate(BaseMessage *message, ErrorResponse **err) {
	Terminate *ret;
	ErrorBuffer err_buff;
	const char *error_message;
	err_buff.offset = 0;


	if(message->type != PSQL_Terminate) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Terminate", message->type, PSQL_Terminate);
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		return NULL;
	}

	ret = (Terminate*)malloc(sizeof(Terminate));
	ret->type = PSQL_Terminate;
	ret->length = sizeof(unsigned int);

	return ret;
}
