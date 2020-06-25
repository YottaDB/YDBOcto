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

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"

CommandComplete *make_command_complete(SqlStatementType type, int32_t rows_sent) {
	CommandComplete *ret;
	int32_t		 length = 0, command_tag_length = 0;
	char		 command_tag[MAX_TAG_LEN];

	// Include row count for SELECT statements, omit for SET and SHOW
	// If not SELECT, SET, or SHOW omit command tag entirely to prevent tags from erroneously appearing in client output.
	// These cases are based on observed client behavior (psql) that does not seem to be documented, so this may need to be
	// updated if this observed behavior changes.
	if (select_STATEMENT == type) {
		snprintf(command_tag, MAX_TAG_LEN, "SELECT %d", rows_sent);
	} else if (set_STATEMENT == type) {
		snprintf(command_tag, MAX_TAG_LEN, "SET");
	} else if (show_STATEMENT == type) {
		snprintf(command_tag, MAX_TAG_LEN, "SHOW");
	} else {
		return NULL;
	}

	length += sizeof(uint32_t);
	command_tag_length = strlen(command_tag) + 1; // count null
	length += command_tag_length;
	ret = (CommandComplete *)malloc(length + sizeof(CommandComplete) - sizeof(uint32_t));
	memset(ret, 0, length + sizeof(CommandComplete) - sizeof(uint32_t));

	ret->type = PSQL_CommandComplete;
	ret->length = htonl(length);
	memcpy(ret->data, command_tag, command_tag_length);

	return ret;
}
