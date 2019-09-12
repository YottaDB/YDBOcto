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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

CancelRequest *read_cancel_request(RoctoSession *session, char *data, int32_t data_length, ErrorResponse **err) {
	CancelRequest *ret = NULL;

	// Currently unused parameters - maintained for consistency across similar read_* functions
	UNUSED(err);

	// Length = length + request_code + pid + secret_key
	uint32_t expected_length = sizeof(uint32_t) + sizeof(int32_t) + sizeof(int32_t) + sizeof(int32_t);
	// Request code format:
	// 	decimal value of most significant 16 bits:  1234
	// 	decimal value of least significant 16 bits: 5678
	int32_t expected_request_code = 80877102;

	// Read all message parameters into return struct
	ret = (CancelRequest*)malloc(sizeof(CancelRequest));
	memset(&ret->length, 0, expected_length);
	memcpy(&ret->length, data, data_length);

	// Convert to host endianness
	ret->length = ntohl(ret->length);
	ret->request_code = ntohl(ret->request_code);

	// Length must be 16 (sum of four ints)
	if(ret->length != expected_length) {
		INFO(ERR_ROCTO_INVALID_INT_VALUE, "CancelRequest", "length", ret->length, sizeof(uint32_t) * 4);
		free(ret);
		return NULL;
	}

	// Request code must match format specified above
	if (expected_request_code != ret->request_code) {
		INFO(ERR_ROCTO_INVALID_INT_VALUE, "CancelRequest", "request code", ret->request_code, 80877102);
		free(ret);
		return NULL;
	}

	// Retrieve remaining fields from cancel request message
	read_bytes(session, (char*)&ret->pid, expected_length - data_length, expected_length - data_length);
	ret->pid = ntohl(ret->pid);
	ret->secret_key = ntohl(ret->secret_key);

	return ret;
}
