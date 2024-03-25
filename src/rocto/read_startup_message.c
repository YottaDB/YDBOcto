/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

StartupMessage *read_startup_message(RoctoSession *session, char **data, int32_t *data_length) {
	StartupMessage *ret = NULL;
	int32_t		num_parms = 0, cur_parm = 0, to_read, tmp_len;
	char	       *c, *message_end, *tmp;
	// Length plus protocol version
	uint32_t hard_coded_ints = sizeof(uint32_t) + sizeof(int);

	// First read length and protocol type, then we will reallocate things
	ret = (StartupMessage *)malloc(sizeof(StartupMessage));
	memcpy(&ret->length, *data, hard_coded_ints);
	ret->length = ntohl(ret->length);
	ret->protocol_version = ntohl(ret->protocol_version);

	// Protocol version number format:
	// 	most significant 16 bits:  major version #, i.e. 3
	// 	least significant 16 bits: minor version #, i.e. 0
	if (0x00030000 != ret->protocol_version) {
		ERROR(ERR_ROCTO_INVALID_VERSION, "StartupMessage", ret->protocol_version, 0x00030000);
		free(ret);
		return NULL;
	}

	// No parameters send
	if (hard_coded_ints == ret->length) {
		ERROR(ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
		free(ret);
		return NULL;
	}

	// Prepare to reallocate
	to_read = ret->length;
	free(ret);
	if (to_read > *data_length) {
		free(*data);
		*data_length = to_read;
		*data = (char *)malloc(sizeof(char) * (*data_length));
	}

	// Size is length in packet + other stuff in the struct, minus the hard-coded
	//  elements in the struct (two int4's)
	ret = (StartupMessage *)malloc(sizeof(StartupMessage) + to_read - hard_coded_ints);
	memcpy(&ret->length, data, hard_coded_ints);
	tmp = (char *)&ret->data;
	tmp_len = to_read - hard_coded_ints;
	read_bytes(session, &tmp, &tmp_len, to_read - hard_coded_ints, FALSE);

	c = ret->data;
	message_end = (char *)(&ret->length) + to_read;

	// Fill out the list of messages; first, count them
	while (c < message_end && *c != '\0') {
		// Read parameter name
		for (; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		if (c == message_end || *c != '\0') {
			ERROR(ERR_ROCTO_MISSING_DATA, "StartupMessage", "parameter name");
			free(ret);
			return NULL;
		}
		c++;
		// Read parameter value
		for (; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		if (c == message_end || *c != '\0') {
			ERROR(ERR_ROCTO_MISSING_NULL, "StartupMessage", "name or value");
			free(ret);
			return NULL;
		}
		c++;
		num_parms++;
	}
	// Ensure parameter list has null terminator
	if (c == message_end || *c != '\0') {
		ERROR(ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
		free(ret);
		return NULL;
	}
	// Skip over the ending \0
	c++;

	// If there are trailing characters, note it
	//  Right now, we will abort the startup, but it's possible to continue in this case
	if (c != message_end) {
		ERROR(ERR_ROCTO_TRAILING_CHARS, "StartupMessage");
		free(ret);
		return NULL;
	}

	// The username is always required: https://www.postgresql.org/docs/11/protocol-message-formats.html
	if (0 == num_parms) {
		ERROR(ERR_ROCTO_MISSING_USERNAME, "read_startup_message");
		free(ret);
		return NULL;
	}

	// Allocate parameter spots, reset c, scan through and populate data
	ret->parameters = (StartupMessageParm *)malloc(sizeof(StartupMessageParm) * num_parms);
	c = ret->data;
	cur_parm = 0;

	while (c < message_end && *c != '\0') {
		// Read parameter name
		ret->parameters[cur_parm].name = c;
		for (; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		// We already checked this message, so don't expect an error case
		assert(*c == '\0');
		c++;
		ret->parameters[cur_parm].value = c;
		// Read parameter value
		for (; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		assert(*c == '\0');
		c++;
		cur_parm++;
	}

	ret->num_parameters = cur_parm;

	return ret;
}
