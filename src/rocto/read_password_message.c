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

PasswordMessage *read_password_message(BaseMessage *message) {
	PasswordMessage *ret;
	uint32_t	 length;
	char *		 c, *message_end;

	length = ntohl(message->length);
	ret = (PasswordMessage *)malloc(sizeof(PasswordMessage) + length - sizeof(uint32_t));
	ret->type = message->type;
	ret->length = length;
	memcpy(ret->data, message->data, length - sizeof(uint32_t));
	c = ret->data;
	message_end = c + length - sizeof(uint32_t);

	// Ensure that message has correct type
	if (ret->type != PSQL_PasswordMessage) {
		ERROR(ERR_ROCTO_INVALID_TYPE, "PasswordMessage", ret->type, PSQL_PasswordMessage);
		free(ret);
		return NULL;
	}
	// Find end of password string
	while (c < message_end && *c != '\0') {
		c++;
	}
	if (c == message_end) {
		// Ensure a password string is included
		if (length == sizeof(uint32_t)) {
			ERROR(ERR_ROCTO_MISSING_DATA, "PasswordMessage", "password");
			free(ret);
			return NULL;
		}
		// Ensure password has null terminator
		ERROR(ERR_ROCTO_MISSING_NULL, "PasswordMessage", "password");
		free(ret);
		return NULL;
	} else if (c < message_end - 1) {
		ERROR(ERR_ROCTO_TRAILING_CHARS, "PasswordMessage");
		free(ret);
		return NULL;
	}
	ret->password = ret->data;

	return ret;
}
