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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

SSLRequest *read_ssl_request(RoctoSession *session, char *data, int data_length, ErrorResponse **err) {
	SSLRequest *ret = NULL;
	ErrorBuffer err_buff;
	const char *error_message;
	err_buff.offset = 0;

	// Currently unused parameters
	UNUSED(session);
	UNUSED(data_length);

	// Length plus request code
	unsigned int expected_length = sizeof(unsigned int) + sizeof(int);
	// Request code format:
	// 	decimal value of most significant 16 bits:  1234
	// 	decimal value of least significant 16 bits: 5679
	int expected_request_code = 80877103;

	// Read length and protocol type
	ret = (SSLRequest*)malloc(sizeof(SSLRequest));
	memcpy(&ret->length, data, expected_length);
	// Convert to host endianness
	ret->length = ntohl(ret->length);
	ret->request_code = ntohl(ret->request_code);

	// Length must be 8 (sum of two ints)
	if(ret->length != expected_length) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_INT_VALUE,
				"SSLRequest", "length", ret->length, "8");
		*err = make_error_response(PSQL_Error_FATAL,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}

	if (expected_request_code != ret->request_code) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_INT_VALUE,
				"SSLRequest", "request code", ret->request_code, "80877103");
		*err = make_error_response(PSQL_Error_FATAL,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}

	return ret;
}
