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

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"
#include "rocto.h"

CommandComplete *make_command_complete(SqlStatementType cmd_type, int32_t row_count) {
	CommandComplete *ret;
	int32_t		 length = 0, command_tag_length = 0;
	char		 command_tag[MAX_TAG_LEN];

	// Include row count for SELECT statements, omit for SET and SHOW
	// If not SELECT, SET, or SHOW omit command tag entirely to prevent tags from erroneously appearing in client output.
	// These cases are based on observed client behavior (psql) that does not seem to be documented, so this may need to be
	// updated if this observed behavior changes.
	switch (cmd_type) {
	case select_STATEMENT:
		snprintf(command_tag, MAX_TAG_LEN, "SELECT %d", row_count); /* row_count is number of rows sent */
		break;
	case insert_STATEMENT:
		snprintf(command_tag, MAX_TAG_LEN, "%s %d", INSERT_COMMAND_TAG, row_count);
		break;
	case delete_from_STATEMENT:
		snprintf(command_tag, MAX_TAG_LEN, "%s %d", DELETE_COMMAND_TAG, row_count);
		break;
	case update_STATEMENT:
		snprintf(command_tag, MAX_TAG_LEN, "%s %d", UPDATE_COMMAND_TAG, row_count);
		break;
	case set_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(SET_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, SET_COMMAND_TAG);
		break;
	case show_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(SHOW_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, SHOW_COMMAND_TAG);
		break;
	case drop_table_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(DROP_TABLE_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, DROP_TABLE_COMMAND_TAG);
		break;
	case drop_view_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(DROP_VIEW_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, DROP_VIEW_COMMAND_TAG);
		break;
	case create_table_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(CREATE_TABLE_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, CREATE_TABLE_COMMAND_TAG);
		break;
	case create_view_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(CREATE_VIEW_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, CREATE_VIEW_COMMAND_TAG);
		break;
	case drop_function_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(DROP_FUNCTION_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, DROP_FUNCTION_COMMAND_TAG);
		break;
	case create_function_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(CREATE_FUNCTION_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, CREATE_FUNCTION_COMMAND_TAG);
		break;
	case truncate_table_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(TRUNCATE_TABLE_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, TRUNCATE_TABLE_COMMAND_TAG);
		break;
	case begin_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(BEGIN_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, BEGIN_COMMAND_TAG);
		break;
	case commit_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(COMMIT_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, COMMIT_COMMAND_TAG);
		break;
	case rollback_STATEMENT:
		assert(MAX_TAG_LEN >= sizeof(ROLLBACK_COMMAND_TAG));
		snprintf(command_tag, MAX_TAG_LEN, ROLLBACK_COMMAND_TAG);
		break;
	default:
		return NULL;
		break;
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
