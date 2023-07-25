/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

CancelRequest *read_cancel_request(RoctoSession *session, char **data, int32_t *data_size) {
	char *	       tmp;
	int	       tmp_len;
	CancelRequest *ret = NULL;
	uint32_t       expected_length = sizeof(CancelRequest);
	uint32_t       data_length = sizeof(pid_t) + sizeof(uint32_t); // pid and secret key
	// Request code format:
	// 	decimal value of most significant 16 bits:  1234
	// 	decimal value of least significant 16 bits: 5678
	int32_t expected_request_code = 80877102;

	if ((int32_t)data_length > *data_size) {
		/* We did not read the minimum bytes needed to check what sort of a message we received. Just return. */
		return NULL;
	}

	// Ensure data buffer has enough space to hold a CancelRequest message.
	if ((int32_t)expected_length > *data_size) {
		char *old_data;

		old_data = *data;
		*data = (char *)malloc(expected_length);
		memcpy(*data, old_data, *data_size);
		*data_size = expected_length;
		free(old_data);
	}
	// Read all message parameters into return struct
	ret = (CancelRequest *)malloc(sizeof(CancelRequest));
	memcpy(ret, *data, expected_length);

	// Convert to host endianness
	ret->length = ntohl(ret->length);
	ret->request_code = ntohl(ret->request_code);

	if (ret->length != expected_length) {
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
	tmp_len = expected_length - data_length;
	tmp = (char *)&ret->pid;
	read_bytes(session, &tmp, &tmp_len, expected_length - data_length, FALSE);
	ret->pid = ntohl(ret->pid);
	ret->secret_key = ntohl(ret->secret_key);

	return ret;
}
