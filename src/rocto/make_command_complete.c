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
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"


CommandComplete *make_command_complete(char *command_tag) {
	CommandComplete *ret;
	char *c;
	int length = 0, command_tag_length = 0;

	// Rather than have special logic for the NULL, just use an empty string
	if(command_tag == NULL)
		command_tag = "";

	length += sizeof(unsigned int);
	command_tag_length = strlen(command_tag) + 1;	// count null
	length += command_tag_length;
	ret = (CommandComplete*)malloc(length + sizeof(CommandComplete) - sizeof(unsigned int));
	memset(ret, 0, length + sizeof(CommandComplete) - sizeof(unsigned int));

	ret->type = PSQL_CommandComplete;
	ret->length = htonl(length);
	memcpy(ret->data, command_tag, command_tag_length);

	return ret;
}
