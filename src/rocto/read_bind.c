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

Bind *read_bind(BaseMessage *message, ErrorResponse **err) {
	Bind *ret;
	ErrorBuffer err_buff;
	char *cur_pointer, *last_byte, *length_ptr;
	const char *error_message;
	unsigned int remaining_length;
	const int default_format_max = 1;
	int i = 0;
	err_buff.offset = 0;

	// Initialize Bind struct
	remaining_length = ntohl(message->length);
	ret = (Bind*)malloc(remaining_length + sizeof(Bind));
	memset(ret, 0, remaining_length + sizeof(Bind));	// prevent leaks
	memcpy(&ret->type, message, remaining_length + 1);	// include type indicator (char)
	remaining_length -= sizeof(unsigned int);		// exclude length from data section

	// Ensure message has correct type
	if(ret->type != PSQL_Bind) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Bind", ret->type, PSQL_Bind);
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}

	// Utility pointers
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;
	// Set destination
	ret->dest = cur_pointer;
	// Ensure destination has null terminator
	while(cur_pointer < last_byte && *cur_pointer != '\0')
		cur_pointer++;
	if(cur_pointer == last_byte || '\0' != *cur_pointer ) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Bind", "destination");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;
	// Set source SQL message
	ret->source = cur_pointer;
	// Ensure source SQL message has null terminator
	while(cur_pointer < last_byte && '\0'!=  *cur_pointer)
		cur_pointer++;
	if(cur_pointer == last_byte || '\0' != *cur_pointer) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Bind", "source");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer++;
	// Set number of parameter format codes and ensure valid value
	ret->num_parm_format_codes = ntohs(*((short int *)cur_pointer));
	if (ret->num_parm_format_codes < 0) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_NUMBER, "Bind", "parameter format codes");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer += sizeof(short int);
	if(cur_pointer > last_byte) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "number of parameter format codes");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	// Set pointer to parameter format codes within data section
	if(ret->num_parm_format_codes > 0)
		ret->parm_format_codes = (short int*)cur_pointer;
	cur_pointer += ret->num_parm_format_codes * sizeof(short int);
	// Ensure all parameter format codes present
	if(cur_pointer > last_byte) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "parameter format codes");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	// Ensure all parameter format codes are valid
	for (i = 0; i < ret->num_parm_format_codes; i++) {
		if (0 != ret->parm_format_codes[i] && 1 != ret->parm_format_codes[i]) {
			error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_INT_VALUE,
					"Bind", "parameter format code", ret->parm_format_codes[i], "0 (text) or 1 (binary)");
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   error_message,
						   0);
			free(ret);
			return NULL;
		}
	}
	// Set number of parameters and ensure valid value
	ret->num_parms = ntohs(*((short int*)cur_pointer));
	if (ret->num_parms < 0) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_NUMBER, "Bind", "parameters");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer += sizeof(short int);
	// Ensure correct number of parameter format codes sent
	if (ret->num_parm_format_codes > ret->num_parms) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TOO_MANY_VALUES, "Bind", "parameter format codes");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret->parms);
		free(ret);
		return NULL;
	}
	if (ret->num_parm_format_codes > default_format_max && ret->num_parm_format_codes != ret->num_parms) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TOO_FEW_VALUES, "Bind", "parameter format codes");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret->parms);
		free(ret);
		return NULL;
	}
	// Ensure parameters are present
	if(cur_pointer > last_byte) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "parameters");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret->parms);
		free(ret);
		return NULL;
	}
	// Read parameters: length of each parameter is unknown in advance, so manually
	// read each parameter and create pointer to its location in the data section
	if(ret->num_parms > 0) {
		ret->parms = (BindParm*)malloc(sizeof(BindParm) * ret->num_parms);
		memset(ret->parms, 0, sizeof(BindParm) * ret->num_parms);	// prevent leaks
		for(i = 0; i < ret->num_parms; i++) {
			// This length does not include the length value; go past that too
			length_ptr = cur_pointer;
			cur_pointer += sizeof(unsigned int);
			if(cur_pointer > last_byte) {
				error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "parameters");
				*err = make_error_response(PSQL_Error_ERROR,
							   PSQL_Code_Protocol_Violation,
							   error_message,
							   0);
				free(ret->parms);
				free(ret);
				return NULL;
			}
			ret->parms[i].length = ntohl(*((long int*)(length_ptr)));
			ret->parms[i].value = cur_pointer;
			cur_pointer += ret->parms[i].length;
			if(cur_pointer > last_byte) {
				error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "parameters");
				*err = make_error_response(PSQL_Error_ERROR,
							   PSQL_Code_Protocol_Violation,
							   error_message,
							   0);
				free(ret->parms);
				free(ret);
				return NULL;
			}
		}
	}
	// Set number of column format codes and ensure correct values
	ret->num_result_col_format_codes = ntohs(*((short int*)cur_pointer));
	if (ret->num_result_col_format_codes < 0) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_NUMBER, "Bind", "result column format codes");
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
		free(ret);
		return NULL;
	}
	cur_pointer += sizeof(short int);
	if(ret->num_result_col_format_codes > 0) {
		ret->result_col_format_codes = (void*)cur_pointer;
		cur_pointer += ret->num_result_col_format_codes * sizeof(short int);
		if(cur_pointer > last_byte) {
			error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA,
					"Bind", "result column format codes");
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   error_message,
						   0);
			free(ret->parms);
			free(ret);
			return NULL;
		}
		// Ensure all column format codes are valid
		for (i = 0; i < ret->num_result_col_format_codes; i++) {
			if (0 != ret->result_col_format_codes[i] && 1 != ret->result_col_format_codes[i]) {
				error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_INT_VALUE,
						"Bind", "result column format code", ret->result_col_format_codes[i],
						"0 (text) or 1 (binary)");
				*err = make_error_response(PSQL_Error_ERROR,
							   PSQL_Code_Protocol_Violation,
							   error_message,
							   0);
				free(ret->parms);
				free(ret);
				return NULL;
			}
		}
	}

	// Verify entire message read
	if(cur_pointer != last_byte) {
		error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "Bind");
		*err = make_error_response(PSQL_Error_WARNING,
					   PSQL_Code_Protocol_Violation,
					   error_message,
					   0);
	}
	return ret;
}
