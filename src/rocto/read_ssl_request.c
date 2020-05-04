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

SSLRequest *read_ssl_request(RoctoSession *session, char *data, int32_t data_length) {
	SSLRequest *ret = NULL;

	// Currently unused parameters
	UNUSED(session);
	UNUSED(data_length);

	// Length plus request code
	uint32_t expected_length = sizeof(uint32_t) + sizeof(int);
	// Request code format:
	// 	decimal value of most significant 16 bits:  1234
	// 	decimal value of least significant 16 bits: 5679
	int32_t expected_request_code = 80877103;

	// Read length and protocol type
	ret = (SSLRequest*)malloc(sizeof(SSLRequest));
	memcpy(&ret->length, data, expected_length);
	// Convert to host endianness
	ret->length = ntohl(ret->length);
	ret->request_code = ntohl(ret->request_code);

	// Length must be 8 (sum of two ints)
	if(ret->length != expected_length) {
		// This message may be useful for debugging, but is not generally useful to users as it provides protocol-level
		// information that is not directly actionable from standard SQL clients.
		TRACE(ERR_ROCTO_INVALID_INT_VALUE, "SSLRequest", "length", ret->length, expected_length);
		free(ret);
		return NULL;
	}

	if (expected_request_code != ret->request_code) {
		// This message may be useful for debugging, but is not generally useful to users as it provides protocol-level
		// information that is not directly actionable from standard SQL clients.
		TRACE(ERR_ROCTO_INVALID_INT_VALUE, "SSLRequest", "request code", ret->request_code, expected_request_code);
		free(ret);
		return NULL;
	}

	return ret;
}
