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

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"

// Creates a basic StartupMessage with only the "user" parameter set for testing purposes.
StartupMessage *make_startup_message(char *username) {
	StartupMessage *ret;
	char *user = "user";
	uint32_t data_len = 0;
	uint32_t user_len = 0;
	uint32_t username_len = 0;

	// Get length of parameter name and value
	user_len = strlen(user) + 1;
	username_len += strlen(username) + 1;
	data_len = user_len + username_len;

	ret = (StartupMessage*)malloc(sizeof(StartupMessage) + data_len);

	// Set length and protocol version
	ret->length = sizeof(uint32_t) + sizeof(int) + data_len;
	ret->protocol_version = 0x00030000;
	ret->num_parameters = 1;
	// Populate data section
	char *c;
	c = ret->data;
	memcpy(c, user, user_len);
	c += user_len;
	memcpy(c, username, username_len);

	// Populate parameter(s)
	ret->parameters = (StartupMessageParm*)malloc(sizeof(StartupMessageParm) * ret->num_parameters);
	ret->parameters[0].name = user;
	ret->parameters[0].value = username;

	return ret;
}
