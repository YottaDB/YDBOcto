/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

Bind *read_bind(BaseMessage *message) {
	Bind *ret;
	char *cur_pointer, *last_byte, *length_ptr;
	uint32_t remaining_length;
	const int32_t default_format_max = 1;
	int32_t i = 0;

	// Initialize Bind struct
	remaining_length = ntohl(message->length);
	ret = (Bind*)malloc(remaining_length + sizeof(Bind));
	memset(ret, 0, remaining_length + sizeof(Bind));	// prevent leaks
	memcpy(&ret->type, message, remaining_length + 1);	// include type indicator (char)
	remaining_length -= sizeof(uint32_t);		// exclude length from data section

	// Ensure message has correct type
	if(ret->type != PSQL_Bind) {
		ERROR(ERR_ROCTO_INVALID_TYPE, "Bind", ret->type, PSQL_Bind);
		free(ret);
		return NULL;
	}

	// Utility pointers
	cur_pointer = ret->data;
	last_byte = cur_pointer + remaining_length;
	// Set destination
	ret->dest = cur_pointer;
	ret->parse_context.parm_start = NULL;
	ret->parse_context.parm_end = NULL;
	// Ensure destination has null terminator
	while(cur_pointer < last_byte && *cur_pointer != '\0')
		cur_pointer++;
	if(cur_pointer == last_byte || '\0' != *cur_pointer ) {
		ERROR(ERR_ROCTO_MISSING_NULL, "Bind", "destination");
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
		ERROR(ERR_ROCTO_MISSING_NULL, "Bind", "source");
		free(ret);
		return NULL;
	}
	cur_pointer++;
	// Set number of parameter format codes and ensure valid value
	ret->num_parm_format_codes = ntohs(*((int16_t *)cur_pointer));
	if (ret->num_parm_format_codes < 0) {
		ERROR(ERR_ROCTO_INVALID_NUMBER, "Bind", "parameter format codes", ret->num_parm_format_codes, 0, INT16_MAX);
		free(ret);
		return NULL;
	}
	cur_pointer += sizeof(int16_t);
	if(cur_pointer > last_byte) {
		ERROR(ERR_ROCTO_MISSING_DATA, "Bind", "number of parameter format codes");
		free(ret);
		return NULL;
	}
	// Set pointer to parameter format codes within data section
	if(ret->num_parm_format_codes > 0)
		ret->parm_format_codes = (int16_t*)cur_pointer;
	cur_pointer += ret->num_parm_format_codes * sizeof(int16_t);
	// Ensure all parameter format codes present
	if(cur_pointer > last_byte) {
		ERROR(ERR_ROCTO_MISSING_DATA, "Bind", "parameter format codes");
		free(ret);
		return NULL;
	}
	// Ensure all parameter format codes are valid
	for (i = 0; i < ret->num_parm_format_codes; i++) {
		ret->parm_format_codes[i] = ntohs(ret->parm_format_codes[i]);
		if (0 != ret->parm_format_codes[i] && 1 != ret->parm_format_codes[i]) {
			ERROR(ERR_ROCTO_INVALID_INT_VALUE_MULTI, "Bind", "parameter format code", ret->parm_format_codes[i], "0 (text) or 1 (binary)");
			free(ret);
			return NULL;
		}
	}
	// Set number of parameters and ensure valid value
	ret->num_parms = ntohs(*((int16_t*)cur_pointer));
	if (ret->num_parms < 0) {
		ERROR(ERR_ROCTO_INVALID_NUMBER, "Bind", "parameters", ret->num_parms, 0, INT16_MAX);
		free(ret);
		return NULL;
	}
	cur_pointer += sizeof(int16_t);
	// Ensure correct number of parameter format codes sent
	if (ret->num_parm_format_codes > ret->num_parms) {
		ERROR(ERR_ROCTO_TOO_MANY_VALUES, "Bind", "parameter format codes");
		free(ret->parms);
		free(ret);
		return NULL;
	}
	if (ret->num_parm_format_codes > default_format_max && ret->num_parm_format_codes != ret->num_parms) {
		ERROR(ERR_ROCTO_TOO_FEW_VALUES, "Bind", "parameter format codes");
		free(ret->parms);
		free(ret);
		return NULL;
	}
	// Ensure parameters are present
	if(cur_pointer > last_byte) {
		ERROR(ERR_ROCTO_MISSING_DATA, "Bind", "parameters");
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
			cur_pointer += sizeof(uint32_t);
			if(cur_pointer > last_byte) {
				ERROR(ERR_ROCTO_MISSING_DATA, "Bind", "parameters");
				free(ret->parms);
				free(ret);
				return NULL;
			}
			ret->parms[i].length = ntohl(*((long int*)(length_ptr)));
			ret->parms[i].value = cur_pointer;
			cur_pointer += ret->parms[i].length;
			if(cur_pointer > last_byte) {
				ERROR(ERR_ROCTO_MISSING_DATA, "Bind", "parameters");
				free(ret->parms);
				free(ret);
				return NULL;
			}
		}
	}
	// Set number of column format codes and ensure correct values
	ret->num_result_col_format_codes = ntohs(*((int16_t*)cur_pointer));
	if (ret->num_result_col_format_codes < 0) {
		ERROR(ERR_ROCTO_INVALID_NUMBER, "Bind", "result column format codes", ret->num_result_col_format_codes, 0, INT16_MAX);
		free(ret);
		return NULL;
	}
	cur_pointer += sizeof(int16_t);
	if(ret->num_result_col_format_codes > 0) {
		ret->result_col_format_codes = (void*)cur_pointer;
		cur_pointer += ret->num_result_col_format_codes * sizeof(int16_t);
		if(cur_pointer > last_byte) {
			ERROR(ERR_ROCTO_MISSING_DATA, "Bind", "result column format codes");
			free(ret->parms);
			free(ret);
			return NULL;
		}
		// Ensure all column format codes are valid
		for (i = 0; i < ret->num_result_col_format_codes; i++) {
			ret->result_col_format_codes[i] = ntohs(ret->result_col_format_codes[i]);
			if (0 != ret->result_col_format_codes[i] && 1 != ret->result_col_format_codes[i]) {
				ERROR(ERR_ROCTO_INVALID_INT_VALUE_MULTI, "Bind", "result column format code", ret->result_col_format_codes[i], "0 (text) or 1 (binary)");
				free(ret->parms);
				free(ret);
				return NULL;
			}
		}
	}

	// Verify entire message read
	if(cur_pointer != last_byte) {
		ERROR(ERR_ROCTO_TRAILING_CHARS, "Bind");
	}
	return ret;
}
